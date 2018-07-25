module Test.NavigationTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import DictTree exposing (Tree(..), get, getPath)
import DictTreeZipper exposing (..)
import Test.SampleData exposing
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
            assertEqual (Just ( (asTree "b" []), [ Context "a" singleChildTree ] ))
                (Just ( singleChildTree, [] )
                    &> goToChild "a"
                )
        , test "Navigate to child (one of many)" <|
            assertEqual
                (Just
                    ( (asTree "c" [])
                    , [ Context "b" multiChildTree
                      ]
                    )
                )
                (Just ( multiChildTree, [] )
                    &> goToChild "b"
                )
        , test "Navigate to a child (deep)" <|
            assertEqual
                (Just
                    ( (asTree "d" [])
                    , [ Context "c" deepTree_c
                      , Context "b" deepTree_b
                      , Context "a" deepTree
                      ]
                    )
                )
                (Just ( deepTree, [] )
                    &> goToChild "a"
                    &> goToChild "b"
                    &> goToChild "c"
                )
--        , test "Navigate to last child of an empty tree returns Nothing" <|
--            assertEqual Nothing
--                (Just ( noChildTree, [] )
--                    &> goToRightMostChild
--                )
--        , test "Navigate to last child of a tree with just one child moves to that child" <|
--            assertEqual
--                (Just ( singleChildTree, [] )
--                    &> goToChild 0
--                )
--                (Just ( singleChildTree, [] )
--                    &> goToRightMostChild
--                )
--        , test "Navigate to last child of a tree with multiple children moves to the last child" <|
--            assertEqual
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToRightMostChild
--                )
--        , test "Navigate to last child of an interestingTree" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 2
--                    &> goToChild 2
--                )
--                (Just ( interestingTree, [] )
--                    &> goToRightMostChild
--                    &> goToRightMostChild
--                )
        , test "Navigate up (single level)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [] ]), [] ))
                (Just ( singleChildTree, [] )
                    &> goToChild "a"
                    &> goUp
                )
        , test "Navigate up (single level with many children)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [], asTree "c" [], asTree "d" [] ]), [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild "b"
                    &> goUp
                )
        , test "Navigate up from a child (deep)" <|
            assertEqual (Just ( (asTree "a" [ asTree "b" [ asTree "c" [ asTree "d" [] ] ] ]), [] ))
                (Just ( deepTree, [] )
                    &> goToChild "a"
                    &> goToChild "b"
                    &> goToChild "c"
                    &> goUp
                    &> goUp
                    &> goUp
                )
        , test "Navigate beyond the tree (only child)" <|
            assertEqual Nothing
                (Just ( singleChildTree, [] )
                    &> goToChild "a"
                    &> goToChild "z"
                )
        , test "Navigate beyond the tree (up past root)" <|
            assertEqual Nothing
                (Just ( singleChildTree, [] )
                    &> goUp
                )
--        , test "Navigate to left sibling on no child tree does not work" <|
--            assertEqual Nothing
--                (Just ( noChildTree, [] )
--                    &> goLeft
--                )
--        , test "Navigate to left child" <|
--            assertEqual
--                (Just ( multiChildTree, [] )
--                    &> goToChild 0
--                    &> goRight
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                    &> goLeft
--                )
--        , test "Navigate to left child twice" <|
--            assertEqual
--                (Just ( multiChildTree, [] )
--                    &> goToChild 0
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                    &> goLeft
--                    &> goLeft
--                )
--        , test "Navigate to left child when there are no siblings left return Nothing" <|
--            assertEqual Nothing
--                (Just ( multiChildTree, [] )
--                    &> goToChild 0
--                    &> goLeft
--                )
--        , test "Navigate to right sibling on no child tree does not work" <|
--            assertEqual Nothing
--                (Just ( noChildTree, [] )
--                    &> goRight
--                )
--        , test "Navigate to right child" <|
--            assertEqual
--                (Just
--                    ( (asTree "c" [])
--                    , [ Context "a"
--                            [ (asTree "b" []) ]
--                            [ (asTree "d" []) ]
--                      ]
--                    )
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToChild 0
--                    &> goRight
--                )
--        , test "Navigate to right child twice" <|
--            assertEqual
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToChild 0
--                    &> goRight
--                    &> goRight
--                )
--        , test "Navigate to right child when there are no siblings left return Nothing" <|
--            assertEqual Nothing
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                    &> goRight
--                )
--        , test "Navigate to next child on Tree with just one node" <|
--            assertEqual Nothing
--                (Just ( noChildTree, [] )
--                    &> goToNext
--                )
--        , test "Navigate to next child on an interesting tree will select the next node" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                    &> goToChild 0
--                )
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                    &> goToNext
--                )
--        , test "Navigate to next child when the end of a branch has been reached will perform backtracking until the next node down can be reached" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 1
--                )
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                    &> goToChild 0
--                    &> goToChild 0
--                    &> goToNext
--                )
--        , test "Navigating past the end of a Tree will return Nothing" <|
--            assertEqual Nothing
--                (Just ( deepTree, [] )
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                )
--        , test "Consecutive goToNext on an interestingTree end up on the right Node" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 2
--                    &> goToChild 1
--                )
--                (Just ( interestingTree, [] )
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                    &> goToNext
--                )
--        , test "Navigate to previous child when there are siblings will select the sibling" <|
--            assertEqual
--                (Just ( multiChildTree, [] )
--                    &> goToChild 1
--                )
--                (Just ( multiChildTree, [] )
--                    &> goToChild 2
--                    &> goToPrevious
--                )
--        , test "Navigate to previous child on an interesting tree will select the previous node" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                )
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                    &> goToChild 0
--                    &> goToPrevious
--                )
--        , test "Navigate to previous child when the beginning of a branch has been reached will perform backtracking until the next node down can be reached" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                    &> goToChild 0
--                    &> goToChild 0
--                )
--                (Just ( interestingTree, [] )
--                    &> goToChild 1
--                    &> goToPrevious
--                )
--        , test "Navigating past the beginning of a Tree will return Nothing" <|
--            assertEqual Nothing
--                (Just ( singleChildTree, [] )
--                    &> goToChild 0
--                    &> goToPrevious
--                    &> goToPrevious
--                )
--        , test "Consecutive goToPrevious on an interestingTree end up on the right Node" <|
--            assertEqual
--                (Just ( interestingTree, [] )
--                    &> goToChild 0
--                )
--                (Just ( interestingTree, [] )
--                    &> goToChild 2
--                    &> goToChild 2
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                    &> goToPrevious
--                )
        , test "Trying to find a non existing element in a Tree returns Nothing" <|
            assertEqual Nothing
                (Just ( interestingTree, [] )
                    &> goTo (\elem -> elem == "FOO")
                )
        , test "Trying to find an existing element in a Tree moves the focus to this element" <|
            assertEqual
                (Just ( interestingTree, [] )
                    &> goToChild "d"
                    &> goToChild "h"
                )
                (Just ( interestingTree, [] )
                    &> goTo (\elem -> elem == "h")
                )
        ]
