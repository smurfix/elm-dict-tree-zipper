module Test.Utils exposing (..)

import Dict
import DictTree exposing (Tree(..), Forest)

(&>) = flip Maybe.andThen

conv : Tree String String -> (String, Tree String String)
conv (Tree datum children) =
    ("_" ++ datum, Tree datum children)

asForest : List (Tree String String) -> Forest String String
asForest list =
    Dict.fromList <| List.map conv list

asTree : String -> List ( Tree String String ) -> Tree String String
asTree datum children =
    Tree datum <| asForest children

