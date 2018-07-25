module Test.AddTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..))
import DictTreeZipper exposing (..)
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

infixl 0 &>>
infixl 0 &>

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = flip Maybe.andThen

(&>>) : Maybe a -> (a -> b) -> Maybe b
(&>>) x f =
    case x of
        Just v ->
            f v |> Just
        Nothing ->
            Nothing

plus1 : number -> number
plus1 x =
    x + 1

foo : (Maybe number) -> (Maybe number)
foo x = 
    x &> Just &>> plus1 &> Just

tests : Test
tests =
    suite "Add"
        [ test "adding children can turn a multiChildTree into an interestingTree" <|
            assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild "Vb"
                    &>> addChild "Ve" (asTree "e" [])
                    &> goToChild "Ve"
                    &>> addChild "Vk" (asTree "k" [])
                    &> goUp
                    &> goToChild "Vc"
                    &>> addChild "Vf" (asTree "f" [])
                    &>> addChild "Vg" (asTree "g" [])
                    &> goToSibling "Vd"
                    &>> addChild "Vh" (asTree "h" [])
                    &>> addChild "Vi" (asTree "i" [])
                    &>> addChild "Vj" (asTree "j" [])
                    &> goToRoot
                )
        ]
