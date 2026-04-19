# STM ringbuffer

## About
This is an implementation of a ring buffer for STM. A wrapper exposing a TBQueue-like interface is provided, offering improved performance compared to the current implementation of TBQueue in the STM package.

This work is similar to https://github.com/haskell/stm/pull/70 . However, this implementation is hopefully correct and supports ringbuffers with size == 0.

## Usage
Mixins can be used to quickly replace the standard TBQueue implementation with the one in this library. Add the following fields to your cabal file:
```
library foo
  build-depends:
    stm
    stm-queue
  mixins:
    stm-queue (Control.Concurrent.STM.TRingBuffer.TBQueue as Control.Concurrent.STM.TBQueue),
    stm hiding (Control.Concurrent.STM.TBQueue)
```

## Benchmark results
In benchmarks, TRingBuffer.TBQueue demonstrates improved performance compared to STM's TBQueue:
```
All
  concurrent spsc
    TBQueue:    OK
      119  ms ± 6.8 ms
    RB.TBQueue: OK
      109  ms ± 7.3 ms
  concurrent mpmc
    TBQueue:    OK
      193  ms ±  14 ms
    RB.TBQueue: OK
      136  ms ± 7.0 ms
  burst
    TBQueue:    OK
      114  ms ± 7.2 ms
    RB.TBQueue: OK
      108  ms ± 6.8 ms
```
