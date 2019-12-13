module Day_1 (answer_1, answer_2) where

import Protolude

-- Part 1

answer_1 :: Int
answer_1 =
    totalFuelRequired fuelRequiredSimple input

fuelRequiredSimple :: Int -> Int
fuelRequiredSimple mass =
    (mass `div` 3) - 2

-- Part 2

answer_2 :: Int
answer_2 =
    totalFuelRequired fuelRequired input

fuelRequired :: Int -> Int
fuelRequired mass =
    fuelRequired' mass 0
    where
        fuelRequired' :: Int -> Int -> Int
        fuelRequired' mass total =
            case fuelRequiredSimple mass of
                fuel | fuel <= 0 -> total
                     | otherwise -> fuelRequired' fuel (total + fuel)

-- Shared

totalFuelRequired :: (Int -> Int) -> [Int] -> Int
totalFuelRequired fuelFn =
    foldr go 0
    where
        go :: Int -> Int -> Int
        go mod total =
            total + fuelFn mod

-- Input

input :: [Int]
input =
    [ 142195
    , 119326
    , 57976
    , 138834
    , 132685
    , 113092
    , 88731
    , 52063
    , 122899
    , 78681
    , 117881
    , 121912
    , 112633
    , 85163
    , 145655
    , 76668
    , 92939
    , 81941
    , 62645
    , 126482
    , 114642
    , 55588
    , 95934
    , 68172
    , 62160
    , 111109
    , 141496
    , 97453
    , 83723
    , 50309
    , 82930
    , 66124
    , 142265
    , 100066
    , 147434
    , 149708
    , 77906
    , 71147
    , 76590
    , 59528
    , 67973
    , 68187
    , 135534
    , 129331
    , 147054
    , 89062
    , 63159
    , 80990
    , 103402
    , 139627
    , 87251
    , 66561
    , 102708
    , 91307
    , 121287
    , 149077
    , 142275
    , 144917
    , 98677
    , 114912
    , 102236
    , 56147
    , 130660
    , 63523
    , 112577
    , 75086
    , 136006
    , 142090
    , 80446
    , 53900
    , 144975
    , 143195
    , 138974
    , 60145
    , 132474
    , 62640
    , 62270
    , 76275
    , 62315
    , 85065
    , 99617
    , 73579
    , 97553
    , 79715
    , 81297
    , 77342
    , 142907
    , 114001
    , 137846
    , 122398
    , 71457
    , 133929
    , 110617
    , 68928
    , 56741
    , 87754
    , 53907
    , 68322
    , 85782
    , 140916
    ]
