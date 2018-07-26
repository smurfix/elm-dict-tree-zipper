module Test.SortTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..), sortBy, sortWith, datum)
import MultiwayTree as T
import Test.Utils exposing (asTree)
import Tuple exposing (second)
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , noChildRecord
        , interestingTree
        )


reverseSortedTree : T.Tree String
reverseSortedTree =
    T.Tree "a"
        [ T.Tree "d"
            [ T.Tree "j" []
            , T.Tree "i" []
            , T.Tree "h" []
            ]
        , T.Tree "c"
            [ T.Tree "g" []
            , T.Tree "f" []
            ]
        , T.Tree "b"
            [ T.Tree "e"
                [ T.Tree "k" [] ]
            ]
        ]


sortedTree : T.Tree String
sortedTree =
    T.Tree "a"
        [ T.Tree "b"
            [ T.Tree "e"
                [ T.Tree "k" [] ]
            ]
        , T.Tree "c"
            [ T.Tree "f" []
            , T.Tree "g" []
            ]
        , T.Tree "d"
            [ T.Tree "h" []
            , T.Tree "i" []
            , T.Tree "j" []
            ]
        ]


myDeepTree : T.Tree String
myDeepTree =
    T.Tree "a"
        [ T.Tree "b"
            [ T.Tree "c"
                [ T.Tree "d" [] ]
            ]
        ]


type alias Seq comparable b =
    ( comparable, Tree comparable b )


flippedComparison : Seq comparable b -> Seq comparable b -> Order
flippedComparison a b =
    case compare (Tuple.first a) (Tuple.first b) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


tests : Test
tests =
    suite "Sort"
        [ test "Sorting a Tree with only one child per levels yields the same Tree" <|
            assertEqual myDeepTree
                (sortBy (\x -> second x |> datum) deepTree)
        , test "Sorting a sorted Tree returns the same Tree" <|
            assertEqual sortedTree
                (sortBy (\x -> second x |> datum) interestingTree)
        , test "Sorting with a Tree with a reversed comperator reverse-sorts a Tree" <|
            assertEqual reverseSortedTree
                (sortWith flippedComparison interestingTree)
        ]
