module Main where

import Protolude

import Test.Tasty (defaultMain, testGroup)

import qualified Spec.Day_3 as Day_3

main :: IO ()
main =
    defaultMain $ testGroup "All" [
        Day_3.spec
    ]
