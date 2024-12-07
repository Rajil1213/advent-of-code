module Main (main) where

import Day01Spec qualified
import Day02Spec qualified
import Day03Spec qualified
import Day04Spec qualified
import Day07Spec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 01 Tests" Day01Spec.spec
  describe "Day 02 Tests" Day02Spec.spec
  describe "Day 03 Tests" Day03Spec.spec
  describe "Day 04 Tests" Day04Spec.spec
  describe "Day 07 Tests" Day07Spec.spec
