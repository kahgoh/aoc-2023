module Poker exposing (..)

import Dict exposing (Dict)


type alias Hand =
    { ranks : List Int
    , bid : Int
    }


joker : Int
joker =
    0


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
            0

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

        handScore =
            Dict.toList freqCounts |> List.sortBy freqOrder |> scoreArranged
    in
    [ handScore ] ++ hand.ranks


freqOrder : ( Int, Int ) -> Int
freqOrder entry =
    case entry of
        -- Give jokers a "high" score to push it to the end to one side
        ( 0, _ ) ->
            100

        ( _, c ) ->
            c


scoreArranged : List ( Int, Int ) -> Int
scoreArranged arranged =
    let
        fiveOfAKind =
            6

        fourOfAKind =
            5

        fullHouse =
            4

        threeOfAKind =
            3

        twoPair =
            2

        onePair =
            1
    in
    case arranged of
        -- Five of a kind
        [ _ ] ->
            fiveOfAKind

        -- Next, check ALL joker combinations first, as the patterns can match the no-joker patterns too
        [ ( _, _ ), ( 0, _ ) ] ->
            fiveOfAKind

        -- Four of a kind with joker
        [ ( _, 1 ), ( _, 3 ), ( 0, 1 ) ] ->
            fourOfAKind

        [ ( _, 1 ), ( _, 2 ), ( 0, 2 ) ] ->
            fourOfAKind

        [ ( _, 1 ), ( _, 1 ), ( 0, 3 ) ] ->
            fourOfAKind

        -- Full house
        [ ( _, 2 ), ( _, 2 ), ( 0, 1 ) ] ->
            fullHouse

        -- Three of a kind
        [ ( _, 1 ), ( _, 1 ), ( _, 1 ), ( 0, 2 ) ] ->
            threeOfAKind

        [ ( _, 1 ), ( _, 1 ), ( _, 2 ), ( 0, 1 ) ] ->
            threeOfAKind

        -- One pair
        [ ( _, 1 ), ( _, 1 ), ( _, 1 ), ( _, 1 ), ( 0, 1 ) ] ->
            onePair

        -- No joker patterns
        -- Four of a kind
        [ ( _, 1 ), ( _, 4 ) ] ->
            fourOfAKind

        -- Full house
        [ ( _, 2 ), ( _, 3 ) ] ->
            fullHouse

        -- Three of a kind
        [ ( _, 1 ), ( _, 1 ), ( _, 3 ) ] ->
            threeOfAKind

        -- Two pairs
        [ ( _, 1 ), ( _, 2 ), ( _, 2 ) ] ->
            twoPair

        -- One pair
        [ ( _, 1 ), ( _, 1 ), ( _, 1 ), ( _, 2 ) ] ->
            onePair

        -- Unrecongised or high card
        _ ->
            0


count : Int -> Dict Int Int -> Dict Int Int
count elem acc =
    let
        existing =
            Dict.get elem acc |> Maybe.withDefault 0
    in
    Dict.insert elem (existing + 1) acc
