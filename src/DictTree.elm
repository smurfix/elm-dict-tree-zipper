module DictTree
    exposing
        ( Tree(..)
        , Forest
        , datum
        , children
        , singular
        , map
        , mapSorted
        , mapTree
        , get
        , getPath
        , filter
        , filterWithChildPrecedence
        , flatten
        , foldr
        , foldl
        , length
        , addChild
        , sortBy
        , sortWith
        )

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.


# Types

@docs Tree, Forest


# Operations

@docs singular, datum, children, get, getPath, foldl, foldr, flatten, filter, filterWithChildPrecedence, length, addChild


# Mapping

@docs map, mapSorted, mapTree


# Sorting

@docs sortBy, sortWith

-}

-- todo: actually learn Elm

import Dict exposing (Dict(..))
import List


-- generate a MultiwayTree when sorting a DictTree

import MultiwayTree as T
import Tuple exposing (first, second)


{-| A type to keep track of datum and children.
-}
type Tree comparable b
    = Tree b (Forest comparable b)


{-| A dict of Trees. Convenient for describing children.
-}
type alias Forest comparable b =
    Dict comparable (Tree comparable b)


{-| Access the datum of the current tree
-}
datum : Tree comparable b -> b
datum (Tree v children) =
    v


{-| A tree consisting of just one datum
-}
singular : b -> Tree comparable b
singular datum =
    Tree datum Dict.empty


{-| Access a child node
-}
get : comparable -> Tree comparable b -> Maybe (Tree comparable b)
get key (Tree _ children) =
    Dict.get key children


{-| Access a grandchild-or-whatever node by key list
-}
getPath : List comparable -> Tree comparable b -> Maybe (Tree comparable b)
getPath keys tree =
    case keys of
        [] ->
            Just tree

        key :: rest ->
            Maybe.andThen (getPath rest) (Dict.get key <| children tree)


{-| Access the children of the current tree
-}
children : Tree comparable b -> Forest comparable b
children (Tree v children) =
    children


{-| Inserts a Tree as one child of another Tree
-}
addChild : comparable -> Tree comparable b -> Tree comparable b -> Tree comparable b
addChild nk nv (Tree v children) =
    Tree v (Dict.insert nk nv children)


{-| Reduce a Tree from the left.
-}
foldl : (v -> b -> b) -> b -> Tree comparable v -> b
foldl f accu (Tree datum children) =
    let
        treeUnwrap k_ (Tree datum_ children_) accu_ =
            Dict.foldl treeUnwrap (f datum_ accu_) children_
    in
        Dict.foldl treeUnwrap (f datum accu) children


{-| Reduce a Tree from the right.
-}
foldr : (v -> b -> b) -> b -> Tree comparable v -> b
foldr f accu (Tree datum children) =
    let
        treeUnwrap k_ (Tree datum_ children_) accu_ =
            f datum_ (Dict.foldr treeUnwrap accu_ children_)
    in
        f datum (Dict.foldr treeUnwrap accu children)


{-| Flattens a Tree into a List where the root is the first element of that list.
-}
flatten : Tree comparable b -> List b
flatten tree =
    foldr (::) [] tree


{-| Return the length of the Tree. Calculated recusively as datum (1) + length of children (n)
Since a DictTree is never empty this function will never return Int < 1.
-}
length : Tree comparable a -> Int
length tree =
    foldr (\_ accu -> accu + 1) 0 tree


{-| Map over one level of the DictTree, with keys
-}
map : ((comparable, Tree comparable a) -> b) -> Forest comparable a -> List b
map fn children =
    List.map fn <| Dict.toList children


{-| Map over one sorted level of the DictTree, with keys
-}
mapSorted : ((comparable, Tree comparable a) -> b) -> ((comparable, Tree comparable a) -> comparable1) -> Forest comparable a -> List b
mapSorted fn sorter children =
    List.map fn <| List.sortBy sorter <| Dict.toList children


{-| Map over a complete DictTree, mangling every element
-}
mapTree : (a -> b) -> Tree comparable a -> Tree comparable b
mapTree fn (Tree datum children) =
    let
        mappedDatum =
            fn datum

        mappedChildren =
            Dict.map (\key val -> mapTree fn val) children
    in
        (Tree mappedDatum mappedChildren)


{-| Filter the DictTree. Keep only elements whose datum satisfy the predicate.
-}
filter : (b -> Bool) -> Tree comparable b -> Maybe (Tree comparable b)
filter predicate tree =
    case tree of
        Tree datum children ->
            if predicate datum then
                filter_ predicate datum children
            else
                Nothing


may_ : comparable -> Maybe b -> Maybe ( comparable, b )
may_ k v =
    case v of
        Nothing ->
            Nothing

        Just v ->
            Just ( k, v )


filter_ : (b -> Bool) -> b -> Forest comparable b -> Maybe (Tree comparable b)
filter_ predicate datum children =
    let
        subfilter =
            (\k (Tree datum v) -> predicate datum)

        dict_filter =
            (\( k, v ) -> may_ k (filter predicate v))
    in
        Just <| Tree datum <| Dict.fromList <| List.filterMap dict_filter <| Dict.toList children


{-| Filter the DictTree. If the predicate is True for a Child the entire path to the root will be part of the result Tree.
-}
filterWithChildPrecedence : (b -> Bool) -> Tree comparable b -> Maybe (Tree comparable b)
filterWithChildPrecedence predicate (Tree datum children) =
    let
        subfilter =
            (\k tree acc ->
                case filterWithChildPrecedence predicate tree of
                    Just tree_ ->
                        Dict.insert k tree_ acc

                    Nothing ->
                        acc
            )

        filtered =
            Dict.foldl subfilter Dict.empty children
    in
        if Dict.isEmpty filtered then
            if predicate datum then
                Just (Tree datum Dict.empty)
            else
                Nothing
        else
            Just (Tree datum filtered)


{-| Sort values by a derived property. Does not alter the nesting structure of
the Tree, that is it does not move Nodes up or down levels.

    All sorting functions get a (key,value) tuple so that they can use any
    or all of the key, the datum, and any subtree attributes they can think of.

    (sortBy first -- key
        Tree "aa"
            Dict.fromList([("b", Tree "bb" Dict.empty
            ),( Tree "dd" Dict.empty
            ),( Tree "cc" Dict.empty
            )]))
    == (Tree "aa"
            Dict.fromList([( Tree "bb" Dict.empty
            ),("c", Tree "cc" Dict.empty
            ),("d", Tree "dd" Dict.empty
            )]))

-}
sortBy : (( comparable, Tree comparable a ) -> comparable1) -> Tree comparable a -> T.Tree a
sortBy fn (Tree datum children) =
    let
        sortedChildren =
            Dict.toList children
                |> List.sortBy (\child -> fn child)
                |> List.map (\kv -> second kv |> sortBy fn)
    in
        (T.Tree datum sortedChildren)


{-| Sort values with a custom comparison function like:

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

    This is also the most general sort function, allowing you
    to define any other.

-}
sortWith : (( comparable, Tree comparable a ) -> ( comparable, Tree comparable a ) -> Order) -> Tree comparable a -> T.Tree a
sortWith comparator (Tree datum children) =
    let
        sortedChildren =
            Dict.toList children
                |> List.sortWith (\first second -> comparator first second)
                |> List.map (\kv -> second kv |> sortWith comparator)

        -- convert to key+val tuples, sort them, extract values, recurse into children
    in
        (T.Tree datum sortedChildren)
