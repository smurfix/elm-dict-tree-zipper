module Test.SampleData exposing (..)

import Dict exposing (Dict(..))
import DictTree exposing (Tree(..), Forest)
import Test.Utils exposing (..)


interestingTree : Tree String String
interestingTree =
    asTree "a"
        [ asTree "b"
            [ asTree "e"
                [ asTree "k" [] ]
            ]
        , asTree "c"
            [ asTree "f" []
            , asTree "g" []
            ]
        , asTree "d"
            [ asTree "h" []
            , asTree "i" []
            , asTree "j" []
            ]
        ]


noChildTree =
    asTree "a" []


noChildRecord =
    Tree { selected = False, expanded = False } Dict.empty


singleChildTree =
    asTree "a"
        [ asTree "b" [] ]


multiChildTree =
    asTree "a"
        [ asTree "b" []
        , asTree "c" []
        , asTree "d" []
        ]


deepTree_c =
    asTree "c"
        [ asTree "d" [] ]


deepTree_b =
    asTree "b"
        [ deepTree_c
        ]


deepTree =
    asTree "a"
        [ deepTree_b
        ]


simpleForest =
    asForest
        [ asTree "x" []
        , asTree "y" []
        , asTree "z" []
        ]
