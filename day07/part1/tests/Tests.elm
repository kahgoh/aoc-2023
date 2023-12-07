module Tests exposing (tests)

import Expect
import InputData
import Poker
import SampleData
import Test exposing (..)


tests : Test
tests =
    describe "Poker"
        [ describe "cards"
            [ test "parse hand with bid" <|
                \_ ->
                    Poker.parseHand "82AQA 100"
                        |> Expect.equal
                            { ranks = [ 8, 2, 14, 12, 14 ]
                            , bid = 100
                            }
            ]
        , describe "scoring"
            [ test "score high card" <|
                \_ ->
                    Poker.scoreHand
                        { ranks = [ 8, 4, 9, 10, 2 ]
                        , bid = 100
                        }
                        |> Expect.equal [ 0, 8, 4, 9, 10, 2 ]
            , test "score one pair card" <|
                \_ ->
                    Poker.scoreHand
                        { ranks = [ 8, 2, 9, 10, 2 ]
                        , bid = 100
                        }
                        |> Expect.equal [ 1, 8, 2, 9, 10, 2 ]
            , test "score two pairs" <|
                \_ ->
                    Poker.scoreHand
                        { ranks = [ 8, 12, 12, 10, 8 ]
                        , bid = 100
                        }
                        |> Expect.equal [ 2, 8, 12, 12, 10, 8 ]
            ]
        , describe "handle data"
            [ test "Read sample content" <|
                \_ ->
                    Poker.score SampleData.data
                        |> Expect.equal 6440
            , test "Read input content" <|
                \_ ->
                    Poker.score InputData.data
                        |> Expect.equal 0
            ]
        ]
