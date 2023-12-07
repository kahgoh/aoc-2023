module Tests exposing (tests)

import Expect
import InputData
import Poker
import SampleData
import Test exposing (..)


runScoreHand : List Int -> List Int
runScoreHand ranking =
    Poker.scoreHand
        { ranks = ranking
        , bid = 100
        }


tests : Test
tests =
    describe "Poker"
        [ describe "cards"
            [ test "parse 82AQA 100" <|
                \_ ->
                    Poker.parseHand "82AQA 100"
                        |> Expect.equal
                            { ranks = [ 8, 2, 14, 12, 14 ]
                            , bid = 100
                            }
            , test "parse TJA2J 509" <|
                \_ ->
                    Poker.parseHand "TJA2J 509"
                        |> Expect.equal
                            { ranks = [ 10, 0, 14, 2, 0 ]
                            , bid = 509
                            }
            ]
        , describe "scoring"
            [ test "score high card" <|
                \_ ->
                    runScoreHand [ 8, 4, 9, 10, 2 ]
                        |> Expect.equal [ 0, 8, 4, 9, 10, 2 ]
            , test "score one pair card" <|
                \_ ->
                    runScoreHand [ 8, 2, 9, 10, 2 ]
                        |> Expect.equal [ 1, 8, 2, 9, 10, 2 ]
            , test "score two pairs" <|
                \_ ->
                    runScoreHand [ 8, 12, 12, 10, 8 ]
                        |> Expect.equal [ 2, 8, 12, 12, 10, 8 ]
            , test "score one pair" <|
                \_ ->
                    runScoreHand [ 3, 2, 10, 3, 13 ]
                        |> Expect.equal [ 1, 3, 2, 10, 3, 13 ]
            , test "score 12J54 pair" <|
                \_ ->
                    runScoreHand [ 1, 2, 0, 5, 4 ] |> Expect.equal [ 1, 1, 2, 0, 5, 4 ]
            , test "score JA27A 3 of a kind" <|
                \_ ->
                    runScoreHand [ 0, 14, 2, 7, 14 ]
                        |> Expect.equal [ 3, 0, 14, 2, 7, 14 ]
            , test "score 8Q1JJ" <|
                \_ ->
                    runScoreHand [ 8, 12, 1, 0, 0 ]
                        |> Expect.equal [ 3, 8, 12, 1, 0, 0 ]
            , test "score 9J779 full house" <|
                \_ ->
                    runScoreHand [ 9, 0, 7, 7, 9 ]
                        |> Expect.equal [ 4, 9, 0, 7, 7, 9 ]
            , test "score 6JJ98 3 of a kind" <|
                \_ ->
                    runScoreHand [ 6, 0, 0, 9, 8 ]
                        |> Expect.equal [ 3, 6, 0, 0, 9, 8 ]
            , test "score 3J9TT 3 of a kind" <|
                \_ -> runScoreHand [ 3, 0, 9, 10, 10]
                    |> Expect.equal [ 3, 3, 0, 9, 10, 10]
            , test "score 1JJ55 4 of a kind" <|
                \_ ->
                    runScoreHand [ 1, 0, 0, 5, 5 ] |> Expect.equal [ 5, 1, 0, 0, 5, 5 ]
            , test "score KTJJT 4 of a kind" <|
                \_ ->
                    runScoreHand [ 13, 10, 0, 0, 10 ]
                        |> Expect.equal [ 5, 13, 10, 0, 0, 10 ]
            , test "score T55J5 4 of a kind" <|
                \_ ->
                    runScoreHand [ 10, 5, 5, 0, 5 ]
                        |> Expect.equal [ 5, 10, 5, 5, 0, 5 ]
            , test "score QQQJA 4 of a kind" <|
                \_ ->
                    runScoreHand [ 12, 12, 12, 0, 14 ]
                        |> Expect.equal [ 5, 12, 12, 12, 0, 14 ]
            , test "score JJ8JJ 5 of a kind" <|
                \_ ->
                    runScoreHand [ 0, 0, 8, 0, 0 ]
                        |> Expect.equal [ 6, 0, 0, 8, 0, 0 ]
            , test "score JJAAA 5 of a kind" <|
                \_ ->
                    runScoreHand [ 0, 0, 14, 14, 14 ]
                        |> Expect.equal [ 6, 0, 0, 14, 14, 14 ]
            ]
        , describe "handle data"
            [ test "Read sample content" <|
                \_ ->
                    Poker.score SampleData.data
                        |> Expect.equal 5905
            , test "Read input content" <|
                \_ ->
                    Poker.score InputData.data
                        |> Expect.equal 0
            ]
        ]
