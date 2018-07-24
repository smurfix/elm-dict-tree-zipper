module Test.FilterTests exposing (..)

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
                (DictTree.filter (\_ -> True) interestingTree)
        , test "Filtering a Tree with a predicate returns a filtered Tree"
            <| assertEqual (Just multiChildTree)
                (DictTree.filter (\e -> e < "e") interestingTree)
        , test "If a subtree contains an element which would evaluate the predicate to True it is still not in the result Tree if the parent datum evaluates to false"
            <| assertEqual Nothing
                (DictTree.filter (\e -> e == "k") interestingTree)
        ]
