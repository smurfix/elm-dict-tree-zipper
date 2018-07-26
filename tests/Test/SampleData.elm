module Test.SampleData exposing (..)

import Dict exposing (Dict(..))
import DictTree exposing (Tree(..), Forest)
import Test.Utils exposing (..)


interestingTree : Tree String String
interestingTree =
    makeTree "a"
        [ makeTree "b"
            [ makeTree "e"
                [ makeTree "k" [] ]
            ]
        , makeTree "c"
            [ makeTree "f" []
            , makeTree "g" []
            ]
        , makeTree "d"
            [ makeTree "h" []
            , makeTree "i" []
            , makeTree "j" []
            ]
        ]


noChildTree =
    makeTree "a" []


noChildRecord =
    Tree { selected = False, expanded = False } Dict.empty


singleChildTree =
    makeTree "a"
        [ makeTree "b" [] ]


multiChildTree =
    makeTree "a"
        [ makeTree "b" []
        , makeTree "c" []
        , makeTree "d" []
        ]


deepTree_c =
    makeTree "c"
        [ makeTree "d" [] ]


deepTree_b =
    makeTree "b"
        [ deepTree_c
        ]


deepTree =
    makeTree "a"
        [ deepTree_b
        ]


simpleForest =
    asForest
        [ makeTree "x" []
        , makeTree "y" []
        , makeTree "z" []
        ]
