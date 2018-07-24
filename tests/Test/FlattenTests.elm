module Test.FlattenTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..))
import DictTreeZipper exposing (..)
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , noChildRecord
        , interestingTree
        )


tests : Test
tests =
    suite "Flatten"
        [ test "Flatten multiChildTree"
            <| assertEqual [ "a", "b", "c", "d" ]
                (DictTree.flatten multiChildTree)
        , test "Flatten deepTree"
            <| assertEqual [ "a", "b", "c", "d" ]
                (DictTree.flatten deepTree)
        , test "Flatten interestingTree"
            <| assertEqual [ "a", "b", "e", "k", "c", "f", "g", "d", "h", "i", "j" ]
                (DictTree.flatten interestingTree)
        ]
