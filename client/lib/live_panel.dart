import 'dart:async';

import 'package:built_collection/built_collection.dart';
import 'package:flutter/material.dart';
import 'package:flutter/cupertino.dart';

import 'package:core/deck.dart';
import 'package:core/message.dart';
import 'package:flutter_utils/widget_modifiers.dart';

import 'deck_panel.dart';
import 'docked_preview.dart';

class LivePanel extends StatefulWidget {
  final Stream<Message> stream;
  final StreamSink<Message> streamSink;

  const LivePanel({
    super.key,
    required this.stream,
    required this.streamSink,
  });

  @override
  LivePanelState createState() => LivePanelState();
}

class LivePanelState extends State<LivePanel> {
  var defaultSettings = BuiltMap<String, DisplaySettings>();
  DeckIndex? deckIndex;

  late final StreamSubscription<Message> _streamSubscription;

  void process(Message message) {
    if (message is ShowMessage) {
      setState(() {
        defaultSettings = message.defaultSettings;
        deckIndex = message.deckIndex;
      });
    } else if (message is CloseMessage) {
      setState(() => deckIndex = null);
    } else {
      throw ArgumentError.value(message, "Message type not recognised");
    }
  }

  @override
  void initState() {
    super.initState();

    _streamSubscription = widget.stream.listen(process);
  }

  @override
  void didUpdateWidget(covariant LivePanel oldWidget) {
    super.didUpdateWidget(oldWidget);

    if (widget.stream != oldWidget.stream) {
      _streamSubscription.cancel();
      _streamSubscription = widget.stream.listen(process);
    }
  }

  @override
  dispose() {
    _streamSubscription.cancel();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraints) {
      return Column(
        children: [
          Text(
            deckIndex?.deck.label ?? "Live",
            style: Theme.of(context).primaryTextTheme.headlineSmall,
          )
              .sized(
                width: double.infinity,
              )
              .container(
                padding: const EdgeInsets.all(20),
                color: CupertinoColors.darkBackgroundGray,
              ),
          Row(children: [
            Column(children: [
              (deckIndex == null
                      ? const Text("Nothing Live")
                          .centered()
                          .background(Colors.black)
                      : DeckPanel(
                          stream: widget.streamSink,
                          defaultSettings: defaultSettings,
                          deckIndex: deckIndex!,
                        ))
                  .expanded(),
              DockedPreview(
                stream: widget.stream,
                defaultSettings: defaultSettings,
              ).constrained(
                BoxConstraints(maxHeight: constraints.maxHeight / 3),
              ),
            ]).expanded(),
            const Icon(Icons.close, color: Colors.black)
                .centered()
                .container(
                  color: const Color.fromARGB(255, 255, 0, 0),
                  width: 40,
                )
                .gestureDetector(
                  onTap: () => widget.streamSink.add(CloseMessage()),
                ),
          ]).expanded(),
        ],
      );
    });
  }
}