module Tests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import Test.DictTreeZipper as DTZ


all : Test
all =
    suite "Elm DictTree.Zipper Tests"
        [ DTZ.tests
        ]


main : Program Never () msg
main =
    runSuite all
