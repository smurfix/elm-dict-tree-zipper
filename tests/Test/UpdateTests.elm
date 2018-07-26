module Test.UpdateTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..), singular)
import Dict
import DictTreeZipper exposing (..)
import Test.Utils exposing (asTree)
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , interestingTree
        , simpleForest
        , noChildRecord
        )
import Test.Utils exposing (..)


tests : Test
tests =
    suite "Update"
        [ test "Update datum (simple)" <|
            assertEqual
                (asZipper <| asTree "ax" [])
                (asZipper noChildTree
                    |> updateDatum (\a -> a ++ "x")
                )
        , test "Update datum (record)" <|
            assertEqual
                (asZipper <| singular { selected = True, expanded = False })
                (asZipper noChildRecord
                    |> updateDatum (\rec -> { rec | selected = True })
                )
        , test "Replace datum (simple)" <|
            assertEqual
                (asZipper <| asTree "x" [])
                (asZipper noChildTree |> replaceDatum "x")
        , test "Replace datum (record)" <|
            assertEqual
                (asZipper <| singular { selected = True, expanded = True })
                (asZipper noChildRecord |> replaceDatum { selected = True, expanded = True })
        , test "Replace children (replace with empty)" <|
            assertEqual
                (asZipper noChildTree)
                (asZipper singleChildTree |> updateChildren Dict.empty)
        , test "Replace children (replace with specific)" <|
            assertEqual
                (Tree "a" simpleForest |> asZipper)
                (interestingTree |> asZipper |> updateChildren simpleForest)
        ]
