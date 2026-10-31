# STM ringbuffer

## About
This is an implementation of a ring buffer for STM. A wrapper exposing a TBQueue-like interface is provided, offering improved performance compared to the current implementation of TBQueue in the STM package.

This work is similar to https://github.com/haskell/stm/pull/70 , but fixes the implementation of flushing and supports ringbuffers with size == 0.

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

## Development

It is recommended to add the following to your cabal.project.local file:
```
tests: true
benchmarks: true
semaphore: true
jobs: $ncpus
```

## Benchmark results
In benchmarks, TRingBuffer.TBQueue demonstrates improved performance compared to STM's TBQueue:
```
All
  concurrent spsc
    TBQueue:    OK
      121  ms ± 7.4 ms
    RB.TBQueue: OK
      110  ms ± 7.0 ms
  concurrent mpmc
    TBQueue:    OK
      194  ms ±  17 ms
    RB.TBQueue: OK
      138  ms ± 7.0 ms
  burst
    TBQueue:    OK
      116  ms ± 7.1 ms
    RB.TBQueue: OK
      109  ms ± 7.6 ms
  write/flush
    TBQueue:    OK
      279  μs ±  13 μs
    RB.TBQueue: OK
      263  μs ±  14 μs
```
System information: M2 Macbook Pro with 16GB RAM and macOS 15.7.2
