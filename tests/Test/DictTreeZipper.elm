module Test.DictTreeZipper exposing (tests)

import Legacy.ElmTest as ElmTest exposing (..)
import Test.NavigationTests
import Test.UpdateTests
import Test.FlattenTests
import Test.FoldTests
import Test.FilterTests
import Test.FilterWithChildPrecedenceTests
import Test.AddTests
import Test.LengthTests
import Test.SortTests


tests : Test
tests =
    suite "DictTree.Zipper"
        [ Test.NavigationTests.tests
        , Test.UpdateTests.tests
        , Test.FlattenTests.tests
        , Test.FoldTests.tests
        , Test.FilterTests.tests
        , Test.FilterWithChildPrecedenceTests.tests
        , Test.AddTests.tests
        , Test.LengthTests.tests
        , Test.SortTests.tests
        ]
