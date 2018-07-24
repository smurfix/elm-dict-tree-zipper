module Tests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import Test.DictTreeZipper as DictTreeZipper


all : Test
all =
    suite "Elm DictTreeZipper Tests"
        [ DictTreeZipper.tests
        ]


main : Program Never () msg
main =
    runSuite all
