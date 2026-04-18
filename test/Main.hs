module Main (main) where

import Test.Hspec ( hspec )
import TestTRingBuffer ( testTRingBuffer )
import ReferenceTest ( referenceTest )

main :: IO ()
main = hspec $ do
  testTRingBuffer
  referenceTest
