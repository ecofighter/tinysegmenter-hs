# tinysementer
Haskell implementation of tinysegmenter, a compact Japanese tokenizer

Machine lerning based compact Japanese tokenizer library.
Original library is written in JavaScript by Taku Kudo.
<http://chasen.org/~taku/software/TinySegmenter>

# benchmark

```
benchmarking tinysegmenter/text/list
time                 65.25 ms   (62.21 ms .. 71.15 ms)
                     0.988 R²   (0.966 R² .. 1.000 R²)
mean                 65.03 ms   (63.94 ms .. 67.87 ms)
std dev              2.857 ms   (1.174 ms .. 4.669 ms)

benchmarking tinysegmenter/text/list(only head)
time                 1.288 μs   (1.279 μs .. 1.298 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.288 μs   (1.279 μs .. 1.302 μs)
std dev              37.33 ns   (23.06 ns .. 62.18 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking tinysegmenter/text/vector
time                 74.60 ms   (73.60 ms .. 75.51 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 75.65 ms   (75.02 ms .. 76.68 ms)
std dev              1.423 ms   (636.4 μs .. 2.273 ms)

benchmarking tinysegmenter/bytestring/list
time                 51.60 ms   (51.17 ms .. 52.00 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 51.74 ms   (51.53 ms .. 52.23 ms)
std dev              599.2 μs   (314.9 μs .. 951.9 μs)

benchmarking tinysegmenter/bytestring/list(only head)
time                 1.244 μs   (1.231 μs .. 1.266 μs)
                     0.996 R²   (0.988 R² .. 1.000 R²)
mean                 1.254 μs   (1.237 μs .. 1.317 μs)
std dev              91.36 ns   (17.48 ns .. 203.2 ns)
variance introduced by outliers: 81% (severely inflated)

benchmarking tinysegmenter/bytestring/vector
time                 59.27 ms   (58.67 ms .. 59.84 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 59.72 ms   (59.39 ms .. 60.13 ms)
std dev              699.5 μs   (536.9 μs .. 865.6 μs)

benchmarking tinysegmenter/lazy bytestring/list
time                 53.48 ms   (48.55 ms .. 56.33 ms)
                     0.985 R²   (0.955 R² .. 1.000 R²)
mean                 59.41 ms   (56.25 ms .. 69.51 ms)
std dev              8.794 ms   (473.8 μs .. 14.90 ms)
variance introduced by outliers: 56% (severely inflated)

benchmarking tinysegmenter/lazy bytestring/list(only head)
time                 1.355 μs   (1.347 μs .. 1.366 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.360 μs   (1.352 μs .. 1.376 μs)
std dev              34.85 ns   (18.03 ns .. 67.38 ns)
variance introduced by outliers: 33% (moderately inflated)

benchmarking tinysegmenter/lazy bytestring/vector
time                 68.63 ms   (67.64 ms .. 69.51 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 69.33 ms   (68.87 ms .. 69.99 ms)
std dev              1.004 ms   (681.0 μs .. 1.485 ms)

Benchmark bench: FINISH
```

The [benchmark text](http://www.genpaku.org/timemachine/timemachineu8j.txt) was
[The Time Machine](https://en.wikipedia.org/wiki/The_Time_Machine) by H.G. Wells,
translated to Japanese by Hiroo Yamagata under the CC BY-SA 2.0 License.
