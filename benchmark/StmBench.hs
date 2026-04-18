-- Adapted from STM benchmarks: https://github.com/haskell/stm/blob/master/bench/ChanBench.hs

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StmBench (stmBench) where

import Control.Concurrent.STM
    ( atomically, newTBQueueIO, readTBQueue, writeTBQueue, TBQueue )
import Control.Concurrent.STM.TRingBuffer.TBQueue qualified as RB
import Control.Concurrent.Async ( async, wait )
import Control.Monad ( replicateM, replicateM_ )
import Test.Tasty (localOption)
import Test.Tasty.Bench
    ( bench, bgroup, defaultMain, whnfAppIO, TimeMode(WallTime) )
import Data.Foldable (traverse_)


stmBench :: IO ()
stmBench = defaultMain
    [ localOption WallTime $ bgroup "concurrent spsc"
        [ bench "TBQueue" $ whnfAppIO (concurrentSpsc @TBQueue) n
        , bench "RB.TBQueue" $ whnfAppIO (concurrentSpsc @RB.TBQueue) n
        ]
    , localOption WallTime $ bgroup "concurrent mpmc"
        [ bench "TBQueue" $ whnfAppIO (concurrentMpmc @TBQueue) n
        , bench "RB.TBQueue" $ whnfAppIO (concurrentMpmc @RB.TBQueue) n
        ]
    , bgroup "burst"
        [ bench "TBQueue" $ whnfAppIO (burst @TBQueue 1000) n
        , bench "RB.TBQueue" $ whnfAppIO (burst @RB.TBQueue 1000) n
        ]
    ]
  where
    n = 2000000

class Channel c where
    newc :: IO (c a)
    readc :: c a -> IO a
    writec :: c a -> a -> IO ()

instance Channel TBQueue where
    newc = newTBQueueIO 4096
    readc c = atomically $ readTBQueue c
    writec c x = atomically $ writeTBQueue c x

instance Channel RB.TBQueue where
    newc = RB.newTBQueueIO 4096
    readc c = atomically $ RB.readTBQueue c
    writec c x = atomically $ RB.writeTBQueue c x

-- concurrent writing and reading with single producer, single consumer
concurrentSpsc :: forall c. (Channel c) => Int -> IO ()
concurrentSpsc n = do
    c :: c Int <- newc
    writer <- async $ replicateM_ n $ writec c 1
    reader <- async $ replicateM_ n $ readc c
    wait writer
    wait reader

-- concurrent writing and reading with multiple producers, multiple consumers
concurrentMpmc :: forall c. (Channel c) => Int -> IO ()
concurrentMpmc n = do
    c :: c Int <- newc
    writers <- replicateM 10 $ async $ replicateM_ (n `div` 10) $ writec c 1
    readers <- replicateM 10 $ async $ replicateM_ (n `div` 10) $ readc c
    traverse_ wait writers
    traverse_ wait readers

-- bursts of bulk writes, then bulk reads
burst :: forall c. (Channel c) => Int -> Int -> IO ()
burst k n = do
    c :: c Int <- newc
    replicateM_ k $ do
        replicateM_ (n `div` k) $ writec c 1
        replicateM_ (n `div` k) $ readc c
