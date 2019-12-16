module Day_2 where

import Protolude

import Data.Sequence (Seq(Empty, (:<|)))
import qualified Data.Sequence as Seq

answer_1 :: Maybe Int
answer_1 =
    run input 1202

answer_2 :: Maybe Int
answer_2 =
    foldr go Nothing [0..9999]
    where
        go :: Int -> Maybe Int -> Maybe Int
        go cur res =
            if run input cur == Just 19690720 then
                Just cur
            else
                res

run :: Seq Int -> Int -> Maybe Int
run instructions code =
    Seq.lookup 0 =<< Seq.lookup 0 (run 0 (Seq.chunksOf 4 instructions'))
    where
        run :: Int -> Seq (Seq Int) -> Seq (Seq Int)
        run n all =
            case runOne all =<< (Seq.lookup n all) of
                Nothing -> all
                Just result -> run (n + 1) result
        noun =
            code `div` 100
        verb =
            code `mod` 100
        instructions' =
            Seq.update 1 noun $ Seq.update 2 verb input

runOne :: Seq (Seq Int) -> Seq Int -> Maybe (Seq (Seq Int))
runOne all cur =
    case cur of
        99 :<| _ ->
            Nothing
        1 :<| in1Pos :<| in2Pos :<| outPos :<| Empty ->
            runOperation (+) in1Pos in2Pos outPos
        2 :<| in1Pos :<| in2Pos :<| outPos :<| Empty ->
            runOperation (*) in1Pos in2Pos outPos
        _ ->
            Nothing
    where
        runOperation :: (Int -> Int -> Int) -> Int -> Int -> Int -> Maybe (Seq (Seq Int))
        runOperation op in1Pos in2Pos outPos = do
            let allFlat = join all
            in1 <- Seq.lookup in1Pos allFlat
            in2 <- Seq.lookup in2Pos allFlat
            pure $ Seq.chunksOf 4 (Seq.update outPos (in1 `op` in2) allFlat)

-- Input
input :: Seq Int
input =
    Seq.fromList
        [ 1
        , 0
        , 0
        , 3
        , 1
        , 1
        , 2
        , 3
        , 1
        , 3
        , 4
        , 3
        , 1
        , 5
        , 0
        , 3
        , 2
        , 1
        , 10
        , 19
        , 1
        , 6
        , 19
        , 23
        , 1
        , 10
        , 23
        , 27
        , 2
        , 27
        , 13
        , 31
        , 1
        , 31
        , 6
        , 35
        , 2
        , 6
        , 35
        , 39
        , 1
        , 39
        , 5
        , 43
        , 1
        , 6
        , 43
        , 47
        , 2
        , 6
        , 47
        , 51
        , 1
        , 51
        , 5
        , 55
        , 2
        , 55
        , 9
        , 59
        , 1
        , 6
        , 59
        , 63
        , 1
        , 9
        , 63
        , 67
        , 1
        , 67
        , 10
        , 71
        , 2
        , 9
        , 71
        , 75
        , 1
        , 6
        , 75
        , 79
        , 1
        , 5
        , 79
        , 83
        , 2
        , 83
        , 10
        , 87
        , 1
        , 87
        , 5
        , 91
        , 1
        , 91
        , 9
        , 95
        , 1
        , 6
        , 95
        , 99
        , 2
        , 99
        , 10
        , 103
        , 1
        , 103
        , 5
        , 107
        , 2
        , 107
        , 6
        , 111
        , 1
        , 111
        , 5
        , 115
        , 1
        , 9
        , 115
        , 119
        , 2
        , 119
        , 10
        , 123
        , 1
        , 6
        , 123
        , 127
        , 2
        , 13
        , 127
        , 131
        , 1
        , 131
        , 6
        , 135
        , 1
        , 135
        , 10
        , 139
        , 1
        , 13
        , 139
        , 143
        , 1
        , 143
        , 13
        , 147
        , 1
        , 5
        , 147
        , 151
        , 1
        , 151
        , 2
        , 155
        , 1
        , 155
        , 5
        , 0
        , 99
        , 2
        , 0
        , 14
        , 0
        ]
