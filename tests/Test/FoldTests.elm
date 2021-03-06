module Test.FoldTests exposing (..)

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
    suite "Fold"
        [ test "Foldl interestingTree into List" <|
            assertEqual (DictTree.flatten interestingTree)
                ((DictTree.foldl (::) [] interestingTree) |> List.reverse)
        ]
