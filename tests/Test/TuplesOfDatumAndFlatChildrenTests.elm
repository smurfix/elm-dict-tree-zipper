module Test.TuplesOfDatumAndFlatChildrenTests exposing (..)

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
    suite "TuplesOfDatumAndFlatChildren"
        [ test "TuplesOfDatumAndFlatChildren multiChildTree" <|
            assertEqual [ ( "a", [ "b", "c", "d" ] ), ( "b", [] ), ( "c", [] ), ( "d", [] ) ]
                (DictTree.tuplesOfDatumAndFlatChildren multiChildTree)
        , test "TuplesOfDatumAndFlatChildren deepTree" <|
            assertEqual [ ( "a", [ "b", "c", "d" ] ), ( "b", [ "c", "d" ] ), ( "c", [ "d" ] ), ( "d", [] ) ]
                (DictTree.tuplesOfDatumAndFlatChildren deepTree)
        , test "TuplesOfDatumAndFlatChildren interestingTree" <|
            assertEqual
                [ ( "a", [ "b", "e", "k", "c", "f", "g", "d", "h", "i", "j" ] )
                , ( "b", [ "e", "k" ] )
                , ( "e", [ "k" ] )
                , ( "k", [] )
                , ( "c", [ "f", "g" ] )
                , ( "f", [] )
                , ( "g", [] )
                , ( "d", [ "h", "i", "j" ] )
                , ( "h", [] )
                , ( "i", [] )
                , ( "j", [] )
                ]
                (DictTree.tuplesOfDatumAndFlatChildren interestingTree)
        ]
