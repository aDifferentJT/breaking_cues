#![feature(closure_lifetime_binder)]

use std::collections::HashMap;
use std::sync::{Arc, LazyLock};

use serde::Deserialize;

use itertools::Itertools;

use http::StatusCode;

use diesel::prelude::*;

use include_dir::include_dir;

use futures::future::BoxFuture;
use futures::{FutureExt, SinkExt, StreamExt};

static DATA_DIR: LazyLock<&'static std::path::Path> = LazyLock::new(|| {
    static PROJECT_DIRS: LazyLock<directories::ProjectDirs> =
        LazyLock::new(|| directories::ProjectDirs::from("", "", "Breaking Cues").unwrap());

    let path = PROJECT_DIRS.data_dir();
    let () = std::fs::create_dir_all(path).unwrap();
    path
});

static DATABASE: LazyLock<std::sync::Mutex<SqliteConnection>> = LazyLock::new(|| {
    use diesel::connection::SimpleConnection;

    let mut connection =
        SqliteConnection::establish(DATA_DIR.join("database").to_str().unwrap()).unwrap();

    let query = r"
        CREATE TABLE IF NOT EXISTS decks (
            title VARCHAR NOT NULL PRIMARY KEY,
            body VARCHAR NOT NULL,
            json VARCHAR NOT NULL
        );
    ";
    connection.batch_execute(query).unwrap();
    std::sync::Mutex::new(connection)
});

diesel::table! {
    decks (title) {
        title -> Text,
        body -> Text,
        json -> Text,
    }
}

#[derive(Debug, Deserialize, Insertable)]
struct Deck {
    title: String,
    body: String,
    json: String,
}

fn save(deck: &Deck) -> QueryResult<()> {
    use crate::decks::dsl::*;
    diesel::insert_into(decks)
        .values(deck)
        .execute(&mut *DATABASE.lock().unwrap())?;
    Ok(())
}

fn delete(title_: &str) -> QueryResult<()> {
    use crate::decks::dsl::*;
    diesel::delete(decks.filter(title.eq(title_))).execute(&mut *DATABASE.lock().unwrap())?;
    Ok(())
}

fn search(query: &str) -> QueryResult<Vec<String>> {
    use crate::decks::dsl::*;
    let like_query = format!("%{query}%");
    decks
        .filter(title.eq(&query))
        .select(json)
        .union(decks.filter(title.like(&like_query)).limit(24).select(json))
        .union(decks.filter(body.like(&like_query)).limit(24).select(json))
        .load(&mut *DATABASE.lock().unwrap())
}

fn response<Body>(status: http::StatusCode, body: Body) -> http::Response<Body> {
    http::Response::builder().status(status).body(body).unwrap()
}

const ASSETS: include_dir::Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../dist");

fn get_asset(path: impl AsRef<std::path::Path>) -> axum::response::Response {
    match ASSETS.get_file(path) {
        Some(file) => response(StatusCode::OK, axum::body::Body::from(file.contents())),
        None => response(StatusCode::NOT_FOUND, axum::body::Body::empty()),
    }
}

fn proxy() -> axum::routing::MethodRouter {
    use axum::extract::Query;

    axum::routing::get(
        |Query(params): Query<HashMap<String, String>>,
         mut req: http::Request<axum::body::Body>| async move {
            match params.get("url") {
                Some(url) => match http::Uri::try_from(url) {
                    Ok(url) => {
                        *req.uri_mut() = url;
                        let headers = req.headers_mut();
                        headers.remove("host");
                        match reqwest::Request::try_from(
                            req.map(|body| reqwest::Body::wrap_stream(body.into_data_stream())),
                        ) {
                            Ok(req) => match reqwest::Client::new().execute(req).await {
                                Ok(res) => res.into(),
                                Err(err) => response(
                                    err.status().unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
                                    err.to_string().into(),
                                ),
                            },
                            Err(err) => response(StatusCode::BAD_REQUEST, err.to_string().into()),
                        }
                    }
                    Err(err) => response(StatusCode::BAD_REQUEST, err.to_string().into()),
                },
                None => response(StatusCode::BAD_REQUEST, reqwest::Body::default()),
            }
        },
    )
}

struct StateUnsync<T> {
    state: T,
    fs: Vec<std::sync::Weak<dyn for<'a> Fn(&'a T) -> BoxFuture<'a, ()> + Send + Sync>>,
}

impl<T> StateUnsync<T> {
    fn new(state: T) -> Self {
        StateUnsync {
            state: state,
            fs: Vec::new(),
        }
    }

    async fn register(
        &mut self,
        f: &Arc<impl for<'a> Fn(&'a T) -> BoxFuture<'a, ()> + Send + Sync + 'static>,
    ) {
        {
            let f = Arc::downgrade(f);
            match self.fs.iter_mut().find(|x| x.strong_count() == 0) {
                Some(x) => *x = f,
                None => self.fs.push(f),
            }
        }
        f(&self.state).await;
    }

    async fn update(&mut self, new_state: T) {
        self.state = new_state;
        for f in &self.fs {
            if let Some(f) = f.upgrade() {
                f(&self.state).await;
            }
        }
    }
}

struct State<T> {
    state: parking_lot::Mutex<StateUnsync<T>>,
}

impl<T> State<T> {
    fn new(state: T) -> Self {
        State {
            state: parking_lot::Mutex::new(StateUnsync::new(state)),
        }
    }

    async fn register(
        &self,
        f: &Arc<impl for<'a> Fn(&'a T) -> BoxFuture<'a, ()> + Send + Sync + 'static>,
    ) {
        self.state.lock().register(f).await
    }

    async fn update(&self, new_state: T) {
        self.state.lock().update(new_state).await
    }
}

fn echo_ws(state: &'static State<String>) -> axum::routing::MethodRouter {
    use axum::extract::ws;
    axum::routing::get(|ws: ws::WebSocketUpgrade| async {
        ws.on_upgrade(|socket| async {
            let (sender, mut receiver) = socket.split();
            let sender = Arc::new(parking_lot::Mutex::new(sender));

            let send_handler = Arc::new(for<'a> move |msg: &'a String| -> BoxFuture<'a, ()> {
                let sender = sender.clone();
                async move {
                    match sender.lock().send(ws::Message::Text(msg.clone())).await {
                        Ok(()) => {}
                        Err(err) => println!("{err}"),
                    }
                }
                .boxed()
            });
            state.register(&send_handler).await;

            while let Some(Ok(msg)) = receiver.next().await {
                if let ws::Message::Text(msg) = msg {
                    state.update(msg).await;
                }
            }

            drop(send_handler); // Make sure it survives hereto
        })
    })
}

async fn serve() {
    let app = axum::Router::new()
        .route(
            "/",
            axum::routing::get(|| async { get_asset("output.html") }),
        )
        .route("/*path", {
            use axum::extract::Path;
            axum::routing::get(|Path(path): Path<String>| async move { get_asset(&path) })
        })
        .route("/proxy", proxy())
        .route(
            "/programme",
            echo_ws({
                static STATE: LazyLock<State<String>> = LazyLock::new(|| State::new("".to_owned()));
                &STATE
            }),
        )
        .route(
            "/live",
            echo_ws({
                static STATE: LazyLock<State<String>> = LazyLock::new(|| State::new("".to_owned()));
                &STATE
            }),
        )
        .route("/deck", {
            use axum::extract::Query;
            use axum::Json;
            axum::routing::put(|Json(deck): Json<Deck>| match save(&deck) {
                Ok(()) => std::future::ready(response(StatusCode::OK, axum::body::Body::empty())),
                Err(err) => {
                    std::future::ready(response(StatusCode::INTERNAL_SERVER_ERROR, err.to_string().into()))
                }
            })
            .delete(|Query(params): Query<HashMap<String, String>>| {
                std::future::ready(match params.get("title") {
                    Some(title) => match delete(title) {
                        Ok(()) => response(StatusCode::OK, axum::body::Body::empty()),
                        Err(err) => response(StatusCode::INTERNAL_SERVER_ERROR, err.to_string().into()),
                    },
                    None => response(StatusCode::BAD_REQUEST, axum::body::Body::empty()),
                })
            })
        })
        .route("/search", {
            use axum::extract::Query;
            axum::routing::get(|Query(params): Query<HashMap<String, String>>| {
                std::future::ready(match params.get("query") {
                    Some(query) => match search(query) {
                        Ok(results) => {
                            response(StatusCode::OK, format!("[{}]", results.into_iter().format(",")).into())
                        }
                        Err(err) => response(StatusCode::INTERNAL_SERVER_ERROR, err.to_string().into()),
                    },
                    None => response(StatusCode::BAD_REQUEST, axum::body::Body::empty()),
                })
            })
        });

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .setup(|_app| {
            tauri::async_runtime::spawn(serve());
            Ok(())
        })
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_fs::init())
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
