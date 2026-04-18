-- |
-- Module      :  Control.Concurrent.STM.TRingUnMkTBQueuefer.TBQueue
--
-- 'TBQueue' is a newtype wrapper around 'TRingBuffer' with an interface
-- that matches that of 'Control.Concurrent.STM.TBQueue' . As such, it can
-- be used as a drop-in replacement, for example with mixins.

module Control.Concurrent.STM.TRingBuffer.TBQueue
  ( TBQueue,
    newTBQueue,
    newTBQueueIO,
    readTBQueue,
    tryReadTBQueue,
    flushTBQueue,
    peekTBQueue,
    tryPeekTBQueue,
    writeTBQueue,
    unGetTBQueue,
    lengthTBQueue,
    isEmptyTBQueue,
    isFullTBQueue,
    capacityTBQueue,
  )
where

import Control.Concurrent.STM.TRingBuffer as TRB
  ( TRingBuffer,
    flushFront,
    isEmpty,
    isFull,
    length,
    new,
    newIO,
    peekFront,
    popFront,
    pushBack,
    pushFront,
    size,
  )
import Control.Monad.STM (STM, orElse)
import Numeric.Natural (Natural)

newtype TBQueue a = MkTBQueue {unMkTBQueue :: TRingBuffer a}
  deriving (Eq)

newTBQueue :: Natural -> STM (TBQueue a)
newTBQueue n
  | n > fromIntegral (maxBound :: Int) = error "TBQueue size cannot exceed maxBound :: Int"
  | otherwise = MkTBQueue <$> new (fromIntegral n)

newTBQueueIO :: Natural -> IO (TBQueue a)
newTBQueueIO n
  | n > fromIntegral (maxBound :: Int) = error "TBQueue size cannot exceed maxBound :: Int"
  | otherwise = MkTBQueue <$> newIO (fromIntegral n)

readTBQueue :: TBQueue a -> STM a
readTBQueue = popFront . unMkTBQueue

tryReadTBQueue :: TBQueue a -> STM (Maybe a)
tryReadTBQueue = try . readTBQueue

flushTBQueue :: TBQueue a -> STM [a]
flushTBQueue = flushFront . unMkTBQueue

peekTBQueue :: TBQueue a -> STM a
peekTBQueue = peekFront . unMkTBQueue

tryPeekTBQueue :: TBQueue a -> STM (Maybe a)
tryPeekTBQueue = try . peekTBQueue

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue = pushBack . unMkTBQueue

unGetTBQueue :: TBQueue a -> a -> STM ()
unGetTBQueue = pushFront . unMkTBQueue

lengthTBQueue :: TBQueue a -> STM Natural
lengthTBQueue = fmap fromIntegral . TRB.length . unMkTBQueue

isEmptyTBQueue :: TBQueue a -> STM Bool
isEmptyTBQueue = isEmpty . unMkTBQueue

isFullTBQueue :: TBQueue a -> STM Bool
isFullTBQueue = isFull . unMkTBQueue

capacityTBQueue :: TBQueue a -> Natural
capacityTBQueue = fromIntegral . size . unMkTBQueue

-- helper function
try :: STM a -> STM (Maybe a)
try m = fmap Just m `orElse` pure Nothing
