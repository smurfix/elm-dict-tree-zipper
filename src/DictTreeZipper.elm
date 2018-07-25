module DictTreeZipper
    exposing
        ( Context(..)
        , Breadcrumbs
        , Zipper
        , asZipper
        , goToChild
        , goToSibling
        -- , goToRightMostChild
        , goUp
        -- , goLeft
        -- , goRight
        , goToRoot
        -- , goToNext
        -- , goToPrevious
        , goTo
        , updateDatum
        , replaceDatum
        , datum
        , maybeDatum
        , addChild
        , updateChildren
        )

{-| A library for navigating and updating immutable trees. The elements in
the tree must have the same type. The trees are implemented in a Huet
Zipper fashion.

# Types
@docs Context, Breadcrumbs, Zipper

# Navigation API
@docs goToChild, goUp, goToRoot, goLeft, goRight, goToNext, goToPrevious, goToRightMostChild, goTo

# Update API
@docs updateDatum, replaceDatum, addChild, updateChildren

# Access API
@docs datum, maybeDatum


# References
[The Zipper, Gerard Huet](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
[Learn You A Haskell, Zippers, Miran Lipovaca](http://learnyouahaskell.com/zippers)

-}

-- TODO: Add more documentation

import List
import Maybe exposing (Maybe(..))
import DictTree exposing (Tree(..), Forest, children, datum, addChild)
import Dict exposing (Dict(..))

(&>) = flip Maybe.andThen


{-| The necessary information needed to reconstruct a DictTree as it is
navigated with a Zipper. This context consists of the key at which the
current node is to be stored and the parent dict it's to be stored in.
-}
type Context comparable b
    = Context comparable (Tree comparable b)


{-| A list of Contexts that is contructed as a DictTree is navigated.
Breadcrumbs are used to retain information about parts of the tree that move out
of focus. As the tree is navigated, the needed Context is pushed onto the list
Breadcrumbs, and they are maintained in the reverse order in which they are
visited
-}
type alias Breadcrumbs comparable b =
    List (Context comparable b)


{-| A structure to keep track of the current Tree, as well as the Breadcrumbs to
allow us to continue navigation through the rest of the tree.
-}
type alias Zipper a b =
    ( Tree a b, Breadcrumbs a b )

{-| Create a zipper for a tree
-}

asZipper : Tree comparable b -> Zipper comparable b
asZipper tree =
    (tree, [])

{-| Extract a tree from its zipper
-}

asTree : Zipper comparable b -> Tree comparable b
asTree (tree, zipper) =
    tree

{-| Move up relative to the current Zipper focus. This allows navigation from a
child to its parent.

    (&>) = flip Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> goUp
-}
goUp : Zipper comparable b -> Maybe (Zipper comparable b)
goUp ( tree, breadcrumbs ) =
    case breadcrumbs of
        (Context key (Tree datum_ siblings)) :: bs ->
            Just ( Tree datum_ <| Dict.insert key tree siblings, bs )

        [] ->
            Nothing


{-| Move down relative to the current Zipper focus. This allows navigation from
a parent to one specific child.

    (&>) = flip Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild "c"
-}
goToChild : comparable -> Zipper comparable b -> Maybe (Zipper comparable b)
goToChild key ( tree, breadcrumbs ) =
    let
        not_key = 
            (\(k, v) -> k /= key)
    in
        case Dict.get key <| children tree of
            Nothing ->
                Nothing

            Just tree_ ->
                Just ( tree_, (Context key tree) :: breadcrumbs )


{-| Move to an adjacent Zipper focus. This allows navigation from
a child to one of its siblings.

    (&>) = flip Maybe.andThen

    simpleTree =
        asTree "a"
            [ asTree "b" []
            , asTree "c" []
            , asTree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild "Vc"
        &> goToSibling "Vb"
-}
goToSibling : comparable -> Zipper comparable b -> Maybe (Zipper comparable b)
goToSibling key zipper =
    goUp zipper &> goToChild key
    -- XXX this can be more efficient, but whatever

{-| Move to the root of the current Zipper focus. This allows navigation from
any part of the tree back to the root.

    (&>) = flip Maybe.andThen

    simpleTree =
        asTree "a"
            [ asTree "b"
                [ asTree "e" [] ]
            , asTree "c" []
            , asTree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild "Vb"
        &> goToChild "Ve"
        &> goToRoot
-}
goToRoot : Zipper comparable b -> Maybe (Zipper comparable b)
goToRoot ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Just ( tree, breadcrumbs )

        otherwise ->
            goUp ( tree, breadcrumbs ) |> Maybe.andThen goToRoot


goToFirst : (b -> Bool) -> List comparable -> Zipper comparable b -> Maybe (Zipper comparable b)
goToFirst predicate keys zipper =
    case keys of
        [] ->
            Nothing
        key :: rest ->
            let
                result = 
                    goToChild key zipper &> goTo predicate 
            in
                case result of
                    Nothing ->
                        goToFirst predicate rest zipper
                    Just res ->
                        Just res
        

{-| Move the focus to the first element for which the predicate is True. If no
such element exists returns Nothing. Search proceeds depth-first.
root of the tree.

    (&>) = flip Maybe.andThen

    simpleTree =
        asTree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goTo (\elem -> elem == "e")
-}

goTo : (b -> Bool) -> Zipper comparable b -> Maybe (Zipper comparable b)
goTo predicate zipper =
        if predicate <| datum zipper then
            Just zipper
        else
            goToFirst predicate (Dict.keys <| children <| Tuple.first zipper) zipper


{-| Update the datum at the current Zipper focus. This allows changes to be made
to a part of a node's datum information, given the previous state of the node.

    (&>) = flip Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> updateDatum (\old -> old ++ "X") -- Appends an X to "b"
        &> goToRoot
-}
updateDatum : (b -> b) -> Zipper comparable b -> Zipper comparable b
updateDatum fn ( Tree datum children, breadcrumbs ) =
    ( Tree (fn datum) children, breadcrumbs )


{-| Replace the datum at the current Zipper focus. This allows complete
replacement of a node's datum information, ignoring the previous state of the
node.

    (&>) = flip Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> replaceDatum "X" -- Replaces "b" with "X"
        &> goToRoot
-}
replaceDatum : b -> Zipper comparable b -> Zipper comparable b
replaceDatum newDatum =
    updateDatum (\_ -> newDatum)


{-| Fully replace the children at the current Zipper focus.
-}
updateChildren : Dict comparable (Tree comparable b) -> Zipper comparable b -> Zipper comparable b
updateChildren newChildren ( Tree datum children, breadcrumbs ) =
    ( Tree datum newChildren, breadcrumbs )


{-| Inserts a Tree as a new child of the Tree at the current focus. Does not move the focus.
-}
addChild : comparable -> Tree comparable b -> Zipper comparable b -> Zipper comparable b
addChild key child ( tree, breadcrumbs ) =
    ( DictTree.addChild key child tree, breadcrumbs )


{-| Access the datum at the current Zipper focus.
-}
datum : Zipper comparable b -> b
datum ( tree, breadcrumbs ) =
    DictTree.datum tree


{-| Access the datum at the current Zipper focus as a Maybe.
-}
maybeDatum : Zipper comparable b -> Maybe b
maybeDatum zipper =
    datum zipper
        |> Just
