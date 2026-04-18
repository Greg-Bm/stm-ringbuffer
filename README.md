# STM ringbuffer

## About
This is an implementation of a ring buffer for STM. A wrapper exposing a TBQueue-like interface is provided, offering improved performance compared to the current implementation of TBQueue in the STM package.

This work is similar to https://github.com/haskell/stm/pull/70 .

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
In benchmarks, TRingBuffer.TBQueue demonstrates improved performance compared to TBQueue, especially when there are multiple producers and consumers. For example:

```
All
  concurrent spsc
    TBQueue:    OK
      119  ms ± 6.8 ms
    RB.TBQueue: OK
      109  ms ± 7.3 ms [1.09x faster]
  concurrent mpmc
    TBQueue:    OK
      193  ms ±  14 ms
    RB.TBQueue: OK
      136  ms ± 7.0 ms [1.42x faster]
  burst
    TBQueue:    OK
      114  ms ± 7.2 ms
    RB.TBQueue: OK
      108  ms ± 6.8 ms [1.06x faster]
```
Above, [] are annotations added by me, calculated from reported figures and rounded to 2 decimal places.
