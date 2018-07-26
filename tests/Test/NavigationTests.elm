module Test.NavigationTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..), get, getPath, singular)
import DictTreeZipper exposing (..)
import Test.SampleData
    exposing
        ( singleChildTree
        , multiChildTree
        , deepTree
        , deepTree_b
        , deepTree_c
        , noChildTree
        , interestingTree
        )
import Test.Utils exposing (..)


tests : Test
tests =
    suite "Navigation"
        [ test "Navigate to child (only child)" <|
            assertEqual (Just ( (asTree "b" []), [ Context "_b" singleChildTree ] ))
                (Just ( singleChildTree, [] )
                    &> goToChild "_b"
                )
        , test "Navigate to child (one of many)" <|
            assertEqual
                (Just
                    ( (asTree "b" [])
                    , [ Context "_b" multiChildTree
                      ]
                    )
                )
                (Just ( multiChildTree, [] )
                    &> goToChild "_b"
                )
        , test "Navigate to a child (deep)" <|
            assertEqual
                (Just
                    ( (asTree "d" [])
                    , [ Context "_d" deepTree_c
                      , Context "_c" deepTree_b
                      , Context "_b" deepTree
                      ]
                    )
                )
                (Just ( deepTree, [] )
                    &> goToPath [ "_b", "_c", "_d" ]
                )
        , test "Navigate up (single level)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [] ]), [] ))
                (Just ( singleChildTree, [] )
                    &> goToChild "_b"
                    &> goUp
                )
        , test "Navigate up (single level with many children)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [], asTree "c" [], asTree "d" [] ]), [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild "_b"
                    &> goUp
                )
        , test "Navigate up from a child (deep)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [ asTree "c" [ asTree "d" [] ] ] ]), [] ))
                (Just ( deepTree, [] )
                    &> goToChild "_b"
                    &> goToChild "_c"
                    &> goToChild "_d"
                    &> goUp
                    &> goUp
                    &> goUp
                )
        , test "Navigate beyond the tree (only child)" <|
            assertEqual Nothing
                (Just ( singleChildTree, [] )
                    &> goToChild "_a"
                    &> goToChild "_z"
                )
        , test "Navigate beyond the tree (up past root)" <|
            assertEqual Nothing
                (Just ( singleChildTree, [] )
                    &> goUp
                )
        , test "Trying to find a non existing element in a Tree returns Nothing" <|
            assertEqual Nothing
                (Just ( interestingTree, [] )
                    &> goTo (\elem -> elem == "FOO")
                )
        , test "Trying to find an existing element in a Tree moves the focus to this element" <|
            assertEqual
                (Just ( interestingTree, [] )
                    &> goToChild "_d"
                    &> goToChild "_h"
                )
                (Just ( interestingTree, [] )
                    &> goTo (\elem -> elem == "h")
                )
        , test "Go to a subdir" <|
            assertEqual
                (get "_b" singleChildTree)
                (Just <| singular "b")
        , test "Go to a deeper subdir" <|
            assertEqual
                (deepTree |> get "_b" &> get "_c")
                (Just deepTree_c)
        , test "Go directly to a deeper subdir" <|
            assertEqual
                (getPath [ "_b", "_c" ] deepTree)
                (Just deepTree_c)
        , test "stay home" <|
            assertEqual
                (getPath [] deepTree)
                (Just deepTree)
        , test "Go nowhere" <|
            assertEqual
                (get "duh" deepTree)
                (Nothing)
        , test "Go nowhere A" <|
            assertEqual
                (getPath [ "duh", "_c" ] deepTree)
                (Nothing)
        , test "Go nowhere B" <|
            assertEqual
                (getPath [ "_b", "duh" ] deepTree)
                (Nothing)
        ]
