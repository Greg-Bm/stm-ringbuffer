-- Simulation that ensures that TRingBuffer.TBQueue matches the behavior of TBQueue

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ReferenceTest (referenceTest) where

import Control.Monad.STM
import Control.Concurrent.STM.TBQueue qualified as A
import Control.Concurrent.STM.TRingBuffer.TBQueue qualified as B
import Data.Functor
import Data.List
import GHC.Generics
import Test.QuickCheck
import Generic.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Monad
import Numeric.Natural (Natural)
import Control.Concurrent.STM.TRingBuffer as RB
import qualified Control.Concurrent.STM.TRingBuffer as RB

referenceTest :: SpecWith ()
referenceTest = describe "Compare to reference behavior" $ do
  describe "TBQueue" $ do
    forM_ [0, 1, 2, 4, 8, 16] $ \size -> do
      let title = "matches reference behavior for size == " <> show size
      prop title $ testImplsMatch size implReferenceTBQueue implRBTBQueue
  describe "TRingBuffer" $ do
    forM_ [0, 1, 2, 4, 8, 16] $ \size -> do
      let title = "matches reference behavior for size == " <> show size
      prop title $ testImplsMatch size implReferenceTBQueue implTRingBuffer
    forM_ [0, 1, 2, 4, 8, 16] $ \size -> do
      let titleR = "(reverse order) matches reference behavior for size == " <> show size
      prop titleR $ testImplsMatch size implReferenceTBQueue implTRingBufferReverse

testImplsMatch :: Natural -> Impl a -> Impl b -> [Command] -> IO ()
testImplsMatch size a b commands = do
  bufA <- newBuf a size
  capacity a bufA `shouldBe` size
  bufB <- newBuf b size
  capacity b bufB `shouldBe` size
  resA <- traverse (runCommand a bufA) commands
  resB <- traverse (runCommand b bufB) commands
  resA `shouldBe` resB

data Command
  = Read
  | TryRead
  | Flush
  | Peek
  | TryPeek
  | Write !Int
  | UnGet !Int
  | Length
  | IsEmpty
  | IsFull
  deriving (Show, Generic)

instance Arbitrary Command where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink


-- using this instead of a typeclass lets us test a ringbuffer both
-- forwards and backwards
data Impl a = MkImpl
  {
    newBuf :: Natural -> IO (a Int),
    runCommand :: a Int -> Command -> IO [Int],
    capacity :: a Int -> Natural
  }

implReferenceTBQueue :: Impl A.TBQueue
implReferenceTBQueue = let
  newBuf = A.newTBQueueIO
  runCommand q = atomically . \case
    Read -> attempt $ A.readTBQueue q
    TryRead -> attempt' $ A.tryReadTBQueue q
    Flush -> A.flushTBQueue q
    Peek -> attempt $ A.peekTBQueue q
    TryPeek -> attempt' $ A.tryPeekTBQueue q
    Write e -> attempt $ A.writeTBQueue q e $> 0
    UnGet e -> attempt $ A.unGetTBQueue q e $> 0
    Length -> singleton . fromIntegral <$> A.lengthTBQueue q
    IsEmpty -> fromBool <$> A.isEmptyTBQueue q
    IsFull -> fromBool <$> A.isFullTBQueue q
  capacity = A.capacityTBQueue
  in MkImpl {..}

implRBTBQueue :: Impl B.TBQueue
implRBTBQueue = let
  newBuf = B.newTBQueueIO
  runCommand q = atomically . \case
    Read -> attempt $ B.readTBQueue q
    TryRead -> attempt' $ B.tryReadTBQueue q
    Flush -> B.flushTBQueue q
    Peek -> attempt $ B.peekTBQueue q
    TryPeek -> attempt' $ B.tryPeekTBQueue q
    Write e -> attempt $ B.writeTBQueue q e $> 0
    UnGet e -> attempt $ B.unGetTBQueue q e $> 0
    Length -> singleton . fromIntegral <$> B.lengthTBQueue q
    IsEmpty -> fromBool <$> B.isEmptyTBQueue q
    IsFull -> fromBool <$> B.isFullTBQueue q
  capacity = B.capacityTBQueue
  in MkImpl {..}

implTRingBuffer :: Impl RB.TRingBuffer
implTRingBuffer = let
  newBuf = RB.newIO . fromIntegral
  runCommand q = atomically . \case
    Read -> attempt $ RB.popFront q
    TryRead -> attempt' $ try $ RB.popFront q
    Flush -> RB.flushFront q
    Peek -> attempt $ RB.peekFront q
    TryPeek -> attempt' $ try $ RB.peekFront q
    Write e -> attempt $ RB.pushBack q e $> 0
    UnGet e -> attempt $ RB.pushFront q e $> 0
    Length -> singleton . fromIntegral <$> RB.length q
    IsEmpty -> fromBool <$> RB.isEmpty q
    IsFull -> fromBool <$> RB.isFull q
  capacity = fromIntegral . RB.size
  in MkImpl {..}

implTRingBufferReverse :: Impl RB.TRingBuffer
implTRingBufferReverse = let
  newBuf = RB.newIO . fromIntegral
  runCommand q = atomically . \case
    Read -> attempt $ RB.popBack q
    TryRead -> attempt' $ try $ RB.popBack q
    Flush -> RB.flushBack q
    Peek -> attempt $ RB.peekBack q
    TryPeek -> attempt' $ try $ RB.peekBack q
    Write e -> attempt $ RB.pushFront q e $> 0
    UnGet e -> attempt $ RB.pushBack q e $> 0
    Length -> singleton . fromIntegral <$> RB.length q
    IsEmpty -> fromBool <$> RB.isEmpty q
    IsFull -> fromBool <$> RB.isFull q
  capacity = fromIntegral . RB.size
  in MkImpl {..}

fromBool :: Num a => Bool -> [a]
fromBool b = [if b then 1 else 0]

attempt :: STM a -> STM [a]
attempt m = fmap singleton m `orElse` pure []

attempt' :: Functor f => f (Maybe a) -> f [a]
attempt' m = maybe [] singleton <$> m

try m = fmap Just m `orElse` pure Nothing
