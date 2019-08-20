module Main (main) where

import Test.QuickCheck

import LinearAlgebra

main :: IO Bool
main = props_LinearAlgebra
