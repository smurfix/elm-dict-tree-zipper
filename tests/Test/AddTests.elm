module Test.AddTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..))
import DictTree.Zipper exposing (..)
import Test.Utils exposing (makeTree)
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
                    &&> addChild "_e" (makeTree "e" [])
                    &> goToChild "_e"
                    &&> addChild "_k" (makeTree "k" [])
                    &> goUp
                    &> goUp
                    &> goToChild "_c"
                    &&> addChild "_f" (makeTree "f" [])
                    &&> addChild "_g" (makeTree "g" [])
                    &> goToSibling "_d"
                    &&> addChild "_h" (makeTree "h" [])
                    &&> addChild "_i" (makeTree "i" [])
                    &&> addChild "_j" (makeTree "j" [])
                    &> goToRoot
                )
        ]
