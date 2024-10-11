module UUID exposing (UUID, fromString, toString)

import Basics.Extra exposing (flip)
import Bit exposing (Bit(..))
import Bits exposing (fromIntSigned, fromIntUnsigned)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import List.Extra as List
import Random exposing (Generator, Seed)
import Task exposing (Task, andThen)
import Time exposing (Posix, posixToMillis)


type UUID
    = UUID Bytes


build : Int -> Int -> Int -> Task x UUID
build randA randB randC =
    Time.now
        |> Task.map
            (\timestamp ->
                UUID <|
                    Bits.toBytes <|
                        List.concat
                            [ fromIntUnsigned 48 <| posixToMillis timestamp
                            , fromIntUnsigned 4 7
                            , fromIntSigned 12 randA
                            , [ I, O ]
                            , fromIntSigned 30 randB
                            , fromIntSigned 32 randC
                            ]
            )


generator : Generator (Task x UUID)
generator =
    let
        int =
            Random.int Random.minInt Random.maxInt
    in
    Random.map3 build int int int


nybbleToHex : Int -> Char
nybbleToHex =
    Maybe.withDefault ' ' << flip List.getAt (String.toList "0123456789ABCDEF")


byteToHex : Decode.Decoder String
byteToHex =
    Decode.unsignedInt8
        |> Decode.map
            (\x ->
                String.fromList
                    [ nybbleToHex <| x // 0x10
                    , nybbleToHex <| modBy 0x10 x
                    ]
            )


bytesToHex : Int -> Decode.Decoder String
bytesToHex n =
    Decode.loop "" <|
        \s ->
            if String.length s >= 2 * n then
                Decode.succeed <| Decode.Done s

            else
                Decode.map (Decode.Loop << (++) s) byteToHex


sequence : List (Decode.Decoder a) -> Decode.Decoder (List a)
sequence xs =
    case xs of
        [] ->
            Decode.succeed []

        y :: ys ->
            y |> Decode.andThen (\z -> Decode.map ((::) z) <| sequence ys)


decodeUUID : Decode.Decoder String
decodeUUID =
    Decode.map (String.join "-") <|
        sequence
            [ bytesToHex 4
            , bytesToHex 2
            , bytesToHex 2
            , bytesToHex 2
            , bytesToHex 6
            ]


toString : UUID -> String
toString (UUID bytes) =
    Maybe.withDefault "" <| Decode.decode decodeUUID bytes


fromString : String -> Maybe UUID
fromString =
    Debug.todo
