-- |
-- Module      :  Control.Concurrent.STM.TRingBuffer
--
-- 'TRingBuffer' is an STM ring buffer. The implementation allows for
-- simultaneous operations on the front and back of the buffer.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Concurrent.STM.TRingBuffer
  ( TRingBuffer,
    new,
    newIO,
    length,
    isEmpty,
    isFull,
    size,
    pushFront,
    pushBack,
    popFront,
    popBack,
    peekFront,
    peekBack,
    flushFront,
    flushBack,
  )
where

import Control.Concurrent.STM
  ( STM,
    TArray,
    TVar,
    newTVar,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Concurrent.STM.TArray ()
import Data.Array.Base
  ( MArray (newArray, unsafeRead, unsafeWrite),
  )
import Data.Array.MArray ()
import Data.DList as DList (empty, snoc, toList)
import Prelude hiding (length, null)

-- | STM ring buffer.
data TRingBuffer a
  = MkTRingBuffer
  { -- | Maximum number of elements the ring buffer can hold.
    size :: {-# UNPACK #-} !Int,
    -- Underlying TArray
    -- index of frontmost item item
    front :: {-# UNPACK #-} !(TVar Int),
    -- index of rearmost item
    back :: {-# UNPACK #-} !(TVar Int),
    arr :: {-# UNPACK #-} !(TArray Int (Maybe a))
  }

instance Eq (TRingBuffer a) where
  (==) :: TRingBuffer a -> TRingBuffer a -> Bool
  a == b = front a == front b

-- | Initialize a ring buffer in STM. O(size)
new :: Int -> STM (TRingBuffer a)
new size = check size $ do
  arr <- newArray (0, size - 1) Nothing
  front <- newTVar $! incr size 0
  back <- newTVar 0
  pure $! MkTRingBuffer {arr, front, back, size}

-- | Initialize a ring buffer in IO. This may be used with UnsafePerformIO. O(size)
newIO :: Int -> IO (TRingBuffer a)
newIO size = check size $ do
  arr <- newArray (0, size - 1) Nothing
  front <- newTVarIO $! incr size 0
  back <- newTVarIO 0
  pure $! MkTRingBuffer {arr, front, back, size}

check :: (Ord a, Num a) => a -> p -> p
check n a =
  if n >= 0
    then a
    else error "Attempted to initialize RingBuffer with size < 0"

-- | Current length. O(1)
length :: TRingBuffer a -> STM Int
length rb@MkTRingBuffer {front, back, size} = do
  rbIsFull <- isFull rb
  if rbIsFull
    then pure size
    else do
      f <- readTVar front
      b <- readTVar back
      pure $ (b + 1 - f) `mod` size

-- | Is the ring buffer empty?
isEmpty :: TRingBuffer a -> STM Bool
isEmpty MkTRingBuffer {arr, front, size} = testPure size True $ do
  ix <- readTVar front
  res <- unsafeRead arr ix
  pure $ case res of
    Nothing -> True
    Just _ -> False

-- | Is the ring buffer full?
isFull :: TRingBuffer a -> STM Bool
isFull MkTRingBuffer {arr, front, size} = testPure size True $ do
  ix <- decr size <$> readTVar front
  res <- unsafeRead arr ix
  pure $ case res of
    Nothing -> False
    Just _ -> True

-- | Push an element to the front of the buffer. Retry if the buffer is full. O(1)
pushFront :: TRingBuffer a -> a -> STM ()
pushFront MkTRingBuffer {arr, front, size} e = testRetry size $ do
  ix <- decr size <$> readTVar front
  cur <- unsafeRead arr ix
  case cur of
    Just _ -> retry
    Nothing -> do
      unsafeWrite arr ix (Just e)
      writeTVar front ix

-- | Push an element to the back of the buffer. Retry if the buffer is full. O(1)
pushBack :: TRingBuffer a -> a -> STM ()
pushBack MkTRingBuffer {arr, back, size} e = testRetry size $ do
  ix <- incr size <$> readTVar back
  cur <- unsafeRead arr ix
  case cur of
    Just _ -> retry
    Nothing -> do
      unsafeWrite arr ix (Just e)
      writeTVar back ix

-- | Pop an element from the front of the buffer. Retry if the buffer is empty. O(1)
popFront :: TRingBuffer b -> STM b
popFront MkTRingBuffer {arr, front, size} = testRetry size $ do
  ix <- readTVar front
  cur <- unsafeRead arr ix
  case cur of
    Nothing -> retry
    Just e -> do
      unsafeWrite arr ix Nothing
      writeTVar front $! incr size ix
      pure e

-- | Pop an element from the back of the buffer. Retry if the buffer is empty. O(1)
popBack :: TRingBuffer b -> STM b
popBack MkTRingBuffer {arr, back, size} = testRetry size $ do
  ix <- readTVar back
  cur <- unsafeRead arr ix
  case cur of
    Nothing -> retry
    Just e -> do
      unsafeWrite arr ix Nothing
      writeTVar back $! decr size ix
      pure e

-- | Peek at the frontmost element of the buffer. Retry if the buffer is empty. O(1)
peekFront :: TRingBuffer b -> STM b
peekFront MkTRingBuffer {arr, front, size} = testRetry size $ do
  ix <- readTVar front
  cur <- unsafeRead arr ix
  maybe retry pure cur

-- | Peek at the backmost element of the buffer. Retry if the buffer is empty. O(1)
peekBack :: TRingBuffer b -> STM b
peekBack MkTRingBuffer {arr, back, size} = testRetry size $ do
  ix <- readTVar back
  cur <- unsafeRead arr ix
  maybe retry pure cur

-- | Pop all elements from the front of the buffer. This operation may succeed even if new elements are pushed to the back in the meantime. O(length)
flushFront :: TRingBuffer a -> STM [a]
flushFront MkTRingBuffer {arr, front, size} = testPure size [] $ do
  initIx <- readTVar front
  go initIx DList.empty
  where
    go !ix !xs = do
      cur <- unsafeRead arr ix
      case cur of
        Nothing -> do
          writeTVar front ix
          pure (toList xs)
        Just x -> do
          unsafeWrite arr ix Nothing
          go (incr size ix) (snoc xs x)

-- | Pop all elements from the back of the buffer. This operation may succeed even if new elements are pushed to the front in the meantime. O(length)
flushBack :: TRingBuffer a -> STM [a]
flushBack MkTRingBuffer {arr, back, size} = testPure size [] $ do
  initIx <- readTVar back
  go initIx DList.empty
  where
    go !ix !xs = do
      cur <- unsafeRead arr ix
      case cur of
        Nothing -> do
          writeTVar back ix
          pure (toList xs)
        Just x -> do
          unsafeWrite arr ix Nothing
          go (decr size ix) (snoc xs x)

-- Functions for moving pointer in ring buffer
-- For n /= 0, these are equivalent to (x \pm 1) `mod` n
-- For n == 0, these are equivalent to (x \pm 1)

incr :: (Eq a, Num a) => a -> a -> a
incr n x = if x == (n - 1) then 0 else x + 1

decr :: (Eq a, Num a) => a -> a -> a
decr n x = if x == 0 then n - 1 else x - 1

-- Functions for handling the case where the buffer's length is zero.
-- Overhead should be negligible when the buffer length is never actually zero.

testRetry :: (Eq a1, Num a1) => a1 -> STM a2 -> STM a2
testRetry !size m = if size == 0 then retry else m

testPure :: (Eq a1, Num a1, Applicative f) => a1 -> a2 -> f a2 -> f a2
testPure !size default' m = if size == 0 then pure default' else m
