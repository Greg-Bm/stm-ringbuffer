# STM ringbuffer

## Motivation
As of April 2026, Control.Concurrent.STM.TBQueue claims that "The implementation is based on an array to obtain O(1) enqueue and dequeue operations", but actually uses a list-based implementation.

In 2023, the PR https://github.com/haskell/stm/pull/70 was made to change the implementation to a ring buffer built on top of a TArray. However, this implementation was buggy, and the implementation of TBQueue was promptly reverted. https://github.com/haskell/stm/issues/76

This is an attempt at a correct and performant implementation of a ring-buffer-based bounded queue. The module `Control.Concurrent.STM.TRingBuffer` offers a general interface to the ring buffer, and the module `Control.Concurrent.STM.TRingBuffer.TBQueue` offers an interface that is mostly compatible with the one for `Control.Concurrent.STM.TBQueue`.

The implementation is somewhat inspired by the above-mentioned PR, but is different.


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
