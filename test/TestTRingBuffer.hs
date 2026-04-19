-- Unit tests for TRingBuffer.
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-do-bind #-}

module TestTRingBuffer (testTRingBuffer) where

import Control.Concurrent.Async ( async )
import Control.Concurrent.STM ( atomically, orElse )
import Control.Concurrent.STM.TRingBuffer as TRB
    ( TRingBuffer(..),
      new,
      isEmpty,
      length,
      isFull,
      newIO,
      pushBack,
      popFront,
      pushFront,
      peekFront,
      flushFront,
      popBack,
      peekBack,
      flushBack )
import Control.Monad
    ( Monad((>>), (>>=)), replicateM, replicateM_, forM_ )
import Data.Functor ( (<&>) )
import Test.Hspec
    ( SpecWith, describe, it, shouldReturn, shouldBe )
import Prelude hiding (length)

testTRingBuffer :: SpecWith ()
testTRingBuffer = describe "Unit tests for TRingBuffer" $ do
  describe "new" $ do
    let newBuf = atomically $ new 10
    it "should be empty upon init" $
      (newBuf >>= atomically . isEmpty) `shouldReturn` True
    it "should have length zero upon init" $
      (newBuf >>= atomically . length) `shouldReturn` 0
    it "should not be full upon init" $
      (newBuf >>= atomically . isFull) `shouldReturn` False
    it "should have size it is initialized with" $
      (newBuf <&> size) `shouldReturn` 10

  describe "newIO" $ do
    let newBuf = newIO 10
    it "should be empty upon init" $
      (newBuf >>= atomically . isEmpty) `shouldReturn` True
    it "should have length zero upon init" $
      (newBuf >>= atomically . length) `shouldReturn` 0
    it "should not be full upon init" $
      (newBuf >>= atomically . isFull) `shouldReturn` False
    it "should have size it is initialized with" $
      (newBuf <&> size) `shouldReturn` 10

  describe "Eq instance" $ do
    it "returns True for same ring buffer" $ do
      buf <- newIO 10 :: IO (TRingBuffer Int)
      (buf == buf) `shouldBe` True
    it "returns False for different ring buffers" $ do
      buf1 <- newIO 10 :: IO (TRingBuffer Int)
      buf2 <- newIO 10
      (buf1 == buf2) `shouldBe` False

  describe "length" $ it "returns length of array" $ do
    buf <- newIO 3
    atomically (length buf) `shouldReturn` 0
    atomically (replicateM 3 (pushBack buf 0 >> length buf))
      `shouldReturn` [1, 2, 3]

  describe "isEmpty" $ it "returns whether array is empty" $ do
    buf <- newIO 3
    atomically (isEmpty buf) `shouldReturn` True
    atomically (replicateM 3 (pushBack buf 0 >> isEmpty buf))
      `shouldReturn` [False, False, False]

  describe "isFull" $ it "returns whether array is full" $ do
    buf <- newIO 3
    atomically (isFull buf) `shouldReturn` False
    atomically (replicateM 3 (pushBack buf 0 >> isFull buf))
      `shouldReturn` [False, False, True]

  describe "pushBack" $ do
    let newBuf = newIO 10
    let pushN n b = replicateM_ n (pushBack b 0)
    it "increases the length" $ do
      buf <- newBuf
      atomically $ pushN 3 buf
      atomically (length buf) `shouldReturn` 3
      atomically $ pushN 7 buf
      atomically (length buf) `shouldReturn` 10
    it "retries when buffer is full" $ do
      buf <- newBuf
      atomically ((pushN 11 buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "popFront" $ do
    let testSeq = [1 .. 10]
    it "takes elements from pushBack in same order" $ do
      buf <- newIO 1
      async $ forM_ testSeq (atomically . pushBack buf)
      replicateM 10 (atomically (popFront buf)) `shouldReturn` testSeq
    it "takes elements from pushFront in reverse order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushFront buf)
      replicateM 10 (atomically (popFront buf)) `shouldReturn` reverse testSeq
    it "retries when buffer is empty" $ do
      buf <- newIO 10
      atomically ((popFront buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "peekFront" $ do
    let testHead = 1
        testTail = [2 .. 10]
    it "looks at the first element pushed back" $ do
      buf <- newIO 10
      forM_ (testHead : testTail) (atomically . pushBack buf)
      atomically (peekFront buf) `shouldReturn` testHead
    it "retries when buffer is empty" $ do
      buf <- newIO 10
      atomically ((popFront buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "flushFront" $ do
    let testSeq = [1 .. 10]
    it "takes elements from pushBack in same order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushBack buf)
      atomically (flushFront buf) `shouldReturn` testSeq
    it "takes elements from pushFront in reverse order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushFront buf)
      atomically (flushFront buf) `shouldReturn` reverse testSeq
    it "leaves the queue empty" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushFront buf)
      atomically (flushFront buf)
      atomically (isEmpty buf) `shouldReturn` True

  describe "pushFront" $ do
    let newBuf = newIO 10
    let pushN n b = replicateM_ n (pushFront b 0)
    it "increases the length" $ do
      buf <- newBuf
      atomically $ pushN 3 buf
      atomically (length buf) `shouldReturn` 3
      atomically $ pushN 7 buf
      atomically (length buf) `shouldReturn` 10
    it "retries when buffer is full" $ do
      buf <- newBuf
      atomically ((pushN 11 buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "popBack" $ do
    let testSeq = [1 .. 10]
    it "takes elements from pushFront in same order" $ do
      buf <- newIO 1
      async $ forM_ testSeq (atomically . pushFront buf)
      replicateM 10 (atomically (popBack buf)) `shouldReturn` testSeq
    it "takes elements from pushBack in reverse order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushBack buf)
      replicateM 10 (atomically (popBack buf)) `shouldReturn` reverse testSeq
    it "retries when buffer is empty" $ do
      buf <- newIO 10
      atomically ((popBack buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "peekBack" $ do
    let testHead = 1
        testTail = [2..10]
    it "looks at the first element pushed front" $ do
      buf <- newIO 10
      forM_ (testHead : testTail) (atomically . pushFront buf)
      atomically (peekBack buf) `shouldReturn` testHead
    it "retries when buffer is empty" $ do
      buf <- newIO 10
      atomically ((popBack buf >> pure True) `orElse` pure False)
        `shouldReturn` False

  describe "flushBack" $ do
    let testSeq = [1 .. 10]
    it "takes elements from pushFront in same order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushFront buf)
      atomically (flushBack buf) `shouldReturn` testSeq
    it "takes elements from pushBack in reverse order" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushBack buf)
      atomically (flushBack buf) `shouldReturn` reverse testSeq
    it "leaves the queue empty" $ do
      buf <- newIO 10
      forM_ testSeq (atomically . pushBack buf)
      atomically (flushBack buf)
      atomically (isEmpty buf) `shouldReturn` True
