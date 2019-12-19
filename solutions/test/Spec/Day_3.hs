module Spec.Day_3 where

import Protolude

import qualified Data.Sequence as Seq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Day_3 (Direction(..), Point(..))
import qualified Day_3

spec :: TestTree
spec =
    testGroup "Day_3"
        [ solve
        , toDirection
        , pathSegment
        , manhattanDistance
        , path
        ]

solve :: TestTree
solve =
    testGroup "solve"
        [ testCase "test 1" $ do
            let input = ("R8,U5,L5,D3", "U7,R6,D4,L4")
            Day_3.solve input @?= Just 6
        , testCase "test 2" $ do
            let input =
                    ( "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                    , "U62,R66,U55,R34,D71,R55,D58,R83"
                    )
            Day_3.solve input @?= Just 159
        , testCase "test 3" $ do
            let input =
                    ( "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                    , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                    )
            Day_3.solve input @?= Just 135
        ]

toDirection :: TestTree
toDirection =
    testGroup "toDirection"
        [ testCase "R1" $
            Day_3.toDirection "R1" @?= Just (R 1)
        , testCase "L20" $
            Day_3.toDirection "L20" @?= Just (L 20)
        , testCase "U123456" $
            Day_3.toDirection "U123456" @?= Just (U 123_456)
        , testCase "D159" $
            Day_3.toDirection "D159" @?= Just (D 159)
        ]

pathSegment :: TestTree
pathSegment =
    testGroup "pathSegment"
        [ testCase "(0, 0), R1" $
            Day_3.pathSegment (Point { x = 0, y = 0 }) (R 1) @?=
                Seq.fromList [Point { x = 1, y = 0 }]
        , testCase "(1, 2), L2" $
            Day_3.pathSegment (Point { x = 1, y = 2 }) (L 2) @?=
                Seq.fromList
                    [ Point { x = 0, y = 2 }
                    , Point { x = -1, y = 2 }
                    ]
        , testCase "(-1, -2), U3" $
            Day_3.pathSegment (Point { x = -1, y = -2 }) (U 3) @?=
                Seq.fromList
                    [ Point { x = -1, y = -1 }
                    , Point { x = -1, y = 0 }
                    , Point { x = -1, y = 1 }
                    ]
        , testCase "(5, 5), D2" $
            Day_3.pathSegment (Point { x = 5, y = 5 }) (D 2) @?=
                Seq.fromList
                    [ Point { x = 5, y = 4 }
                    , Point { x = 5, y = 3 }
                    ]
        , testCase "(0, 0), R0" $
            Day_3.pathSegment (Point { x = 0, y = 0 }) (R 0) @?= Seq.empty
        ]

manhattanDistance :: TestTree
manhattanDistance =
    testGroup "manhattanDistance"
        [ testCase "(0, 0), (1, 1)" $
            Day_3.manhattanDistance (Point { x = 0, y = 0 }) (Point { x = 1, y = 1}) @?= 2
        , testCase "(0, 0), (-1, -1)" $
            Day_3.manhattanDistance (Point { x = 0, y = 0 }) (Point { x = -1, y = -1}) @?= 2
        , testCase "(-15, 8), (8, 4)" $
            Day_3.manhattanDistance (Point { x = -15, y = 8 }) (Point { x = 8, y = 4}) @?= 27
        ]

path :: TestTree
path =
    testGroup "path"
        [ testCase "R1" $
            Day_3.path [R 1] @?= Seq.fromList [Point { x = 1, y = 0 }]
        , testCase "R1,L2,U3,D1" $
            Day_3.path [R 1, L 2, U 3, D 1] @?=
                Seq.fromList
                    [ Point { x = 1, y = 0 }
                    , Point { x = 0, y = 0 }
                    , Point { x = -1, y = 0 }
                    , Point { x = -1, y = 1 }
                    , Point { x = -1, y = 2 }
                    , Point { x = -1, y = 3 }
                    , Point { x = -1, y = 2 }
                    ]
        ]

wirePaths :: TestTree
wirePaths =
    testGroup "wirePaths"
        [ testCase "('R1,U1', 'L1,D1')" $
            Day_3.wirePaths ("R1,U1", "L1,D1") @?=
                ( Seq.fromList [Point { x = 1, y = 0 }, Point { x = 1, y = 1 }]
                , Seq.fromList [Point { x = -1, y = 0 }, Point { x = -1, y = -1 }]
                )
        ]
