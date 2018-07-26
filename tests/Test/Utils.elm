module Test.Utils exposing (..)

import Dict
import DictTree exposing (Tree(..), Forest)


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen


(&&>) : Maybe a -> (a -> b) -> Maybe b
(&&>) x f =
    case x of
        Just v ->
            f v |> Just

        Nothing ->
            Nothing


infixl 0 &>


infixl 0 &&>


conv : Tree String String -> ( String, Tree String String )
conv (Tree datum children) =
    ( "_" ++ datum, Tree datum children )


asForest : List (Tree String String) -> Forest String String
asForest list =
    Dict.fromList <| List.map conv list


makeTree : String -> List (Tree String String) -> Tree String String
makeTree datum children =
    Tree datum <| asForest children
