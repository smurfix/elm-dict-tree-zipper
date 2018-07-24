module Test.AddTests exposing (..)

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
    suite "Add"
        [ test "adding children can turn a multiChildTree into an interestingTree" <|
            assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild
                        0
                    &> addChild (Tree "e" [])
                    &> goToChild 0
                    &> addChild (Tree "k" [])
                    &> goUp
                    &> goRight
                    &> addChild (Tree "f" [])
                    &> addChild (Tree "g" [])
                    &> goRight
                    &> addChild (Tree "h" [])
                    &> addChild (Tree "i" [])
                    &> addChild (Tree "j" [])
                    &> goToRoot
                )
        ]
