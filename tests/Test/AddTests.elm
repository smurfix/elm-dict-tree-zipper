module Test.AddTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..))
import DictTree.Zipper exposing (..)
import Test.Utils exposing (asTree)
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


plus1 : number -> number
plus1 x =
    x + 1


foo : Maybe number -> Maybe number
foo x =
    x &> Just &&> plus1 &> Just


tests : Test
tests =
    suite "Add"
        [ test "adding children can turn a multiChildTree into an interestingTree" <|
            assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild "_b"
                    &&> addChild "_e" (asTree "e" [])
                    &> goToChild "_e"
                    &&> addChild "_k" (asTree "k" [])
                    &> goUp
                    &> goUp
                    &> goToChild "_c"
                    &&> addChild "_f" (asTree "f" [])
                    &&> addChild "_g" (asTree "g" [])
                    &> goToSibling "_d"
                    &&> addChild "_h" (asTree "h" [])
                    &&> addChild "_i" (asTree "i" [])
                    &&> addChild "_j" (asTree "j" [])
                    &> goToRoot
                )
        ]
