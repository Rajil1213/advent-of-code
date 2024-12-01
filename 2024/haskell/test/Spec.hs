module Main (main) where

import Day01Spec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 01 Tests" Day01Spec.spec
