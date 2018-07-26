# DictTree.Zipper

This library was created with the goal of allowing a dict-based tree to be
navigated and updated. Keys can be any ``comparable``, values can be any
type.

Sorting is supported and will return a ``MultiWayTree``.

## Basic Usage

::

    import DictTree exposing(..)
    (&>) = flip Maybe.andThen

    t = singular "foo"
    >>> Tree "foo" (Dict.fromList []) : DictTree.Tree comparable String

    datum t
    >>> "foo" : String

    t2 = addChild "xx" (singular "bar") t
    >>> Tree "foo" (Dict.fromList [("xx",Tree "bar" (Dict.fromList []))])
            : DictTree.Tree String String
    get "xx" t2 &> datum
    >>> Just "bar"

A zipper allows you to descend into a subdirectory, modify things, and go back.

::
    import DictTree.Zipper as Z

    t3 = t2 |> asZipper |> Z.goToChild "xx"
        &> addChild "yy" (singular "baz") &> goToRoot
    datum t3
    >>> "foo"
    t3 &> Z.asTree &> getPath ["xx","yy"] &> datum
    >>> Just "baz"
    t3 &> Z.goToPath ["xx","yy"] &> datum
    >>> Just "baz"

though the second way is inefficient if you don't intend to modify the data.

### Run tests (from a cloned repo)
    npm install -g elm
    cd tests
    elm-package install -y
    elm-make Tests.elm --output tests.js
    node tests.js
    
The tests in this package are still based on ``rtfeldman/legacy-elm-test``
and thus not compatible with modern test runners. This will be fixed in a
future version.
