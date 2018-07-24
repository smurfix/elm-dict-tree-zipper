module Test.AppendTests exposing (..)

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
import Test.Utils exposing (..)


tests : Test
tests =
    suite "Append"
        [ test "appending children can turn a multiChildTree into an interestingTree" <|
            assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild
                        0
                    &> appendChild (Tree "e" [])
                    &> goToChild 0
                    &> appendChild (Tree "k" [])
                    &> goUp
                    &> goRight
                    &> appendChild (Tree "f" [])
                    &> appendChild (Tree "g" [])
                    &> goRight
                    &> appendChild (Tree "h" [])
                    &> appendChild (Tree "i" [])
                    &> appendChild (Tree "j" [])
                    &> goToRoot
                )
        ]
