# DictTreeZipper

This library was created with the goal of allowing a dict-based tree to be
navigated and updated.

This is a mod of MultiWayTreeZipper, except that the keys to access
an individual node are lists of paths instead of a key that's searched in
the whole tree -- that doesn't scale.

# Run tests (from a cloned repo)
    npm install -g elm
    cd tests
    elm-package install -y
    elm-make Tests.elm --output tests.js
    node tests.js
    
NOTE: elm-test Tests.elm -c elm-make.cmd might have to be used if running on Windows.
