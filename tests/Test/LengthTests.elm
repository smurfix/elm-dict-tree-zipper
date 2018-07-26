module Test.LengthTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..))
import DictTree.Zipper exposing (..)
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
    suite "Length"
        [ test "Length of an interesting Tree" <|
            assertEqual 11
                (DictTree.length interestingTree)
        , test "Length of a noChildTree" <|
            assertEqual 1
                (DictTree.length noChildTree)
        , test "Length of a deepTree" <|
            assertEqual 4
                (DictTree.length deepTree)
        , test "Length of a Tree is equal to length of a flattened tree" <|
            assertEqual (List.length (DictTree.flatten interestingTree))
                (DictTree.length interestingTree)
        ]
