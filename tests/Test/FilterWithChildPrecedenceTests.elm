module Test.FilterWithChildPrecedenceTests exposing (..)

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


tests : Test
tests =
    suite "Filter"
        [ test "Filtering a Tree with a predicate that always returns true returns the same tree"
            <| assertEqual (Just interestingTree)
                (DictTree.filterWithChildPrecedence (\_ -> True) interestingTree)
        , test "Filtering a Tree with a predicate returns a filtered Tree"
            <| assertEqual (Just multiChildTree)
                (DictTree.filterWithChildPrecedence (\e -> e < "e") interestingTree)
        , test "If an element is no where to be found in the tree returns Nothing"
            <| assertEqual Nothing
                (DictTree.filterWithChildPrecedence (\e -> e == "fooo") interestingTree)
        , test "If a predicate evaluates to False for a Node but True for one of it's children then the Node will remain in the Tree"
            <| assertEqual
                (Just
                    (Tree "a"
                        [ Tree "b"
                            [ Tree "e"
                                [ Tree "k" [] ]
                            ]
                        ]
                    )
                )
                (DictTree.filterWithChildPrecedence (\e -> e == "k") interestingTree)
        ]
