module Poker exposing (..)

import Dict exposing (Dict)


type alias Hand =
    { ranks : List Int
    , bid : Int
    }


score : String -> Int
score x =
    String.lines x
        |> List.filter (\content -> content /= "")
        |> List.map parseHand
        |> List.sortBy scoreHand
        |> List.indexedMap (\index hand -> (index + 1) * hand.bid)
        |> List.sum


parseHand : String -> Hand
parseHand text =
    case String.words text of
        rankText :: bidText :: _ ->
            { ranks = String.split "" rankText |> List.map getRank
            , bid = String.toInt bidText |> Maybe.withDefault 0
            }

        _ ->
            { ranks = []
            , bid = 0
            }


getRank : String -> Int
getRank card =
    case card of
        "2" ->
            2

        "3" ->
            3

        "4" ->
            4

        "5" ->
            5

        "6" ->
            6

        "7" ->
            7

        "8" ->
            8

        "9" ->
            9

        "T" ->
            10

        "J" ->
            11

        "Q" ->
            12

        "K" ->
            13

        "A" ->
            14

        _ ->
            0


scoreHand : Hand -> List Int
scoreHand hand =
    let
        freqCounts =
            List.foldr count Dict.empty hand.ranks

        orderedFreq =
            Dict.toList freqCounts |> List.sortBy Tuple.second

        handScore =
            case orderedFreq of
                -- Five of a kind
                [ _ ] ->
                    6

                -- Four of a kind
                [ ( _, 1 ), ( _, 4 ) ] ->
                    5

                -- Full house
                [ ( _, 2 ), ( _, 3 ) ] ->
                    4

                -- Three of a kind
                [ ( _, 1 ), ( _, 1 ), ( _, 3 ) ] ->
                    3

                -- Two pairs
                [ ( _, 1 ), ( _, 2 ), ( _, 2 ) ] ->
                    2

                -- One pair
                [ ( _, 1 ), ( _, 1 ), ( _, 1 ), ( _, 2 ) ] ->
                    1

                -- Unrecongised or high card
                _ ->
                    0
    in
    [ handScore ] ++ hand.ranks


count : Int -> Dict Int Int -> Dict Int Int
count elem acc =
    let
        existing =
            Dict.get elem acc |> Maybe.withDefault 0
    in
    Dict.insert elem (existing + 1) acc
