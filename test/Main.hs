module Main (main) where

import Test.Hspec
import TestTRingBuffer
import ReferenceTest
main :: IO ()
main = hspec $ do
  testTRingBuffer
  referenceTest
