## Requirements
1. [psmmodels](http://www.tcs.hut.fi/Software/smodels/priority/)
2. [lpod2asprin](https://github.com/zhunyoung/lpod2asprin)

The `lparse` and `psmodels` executables as well as the `lpod2asprin` files need to be placed in the benchmarks directory, in order for the systems tests to be able to run.

## Running the tests
Make sure all scripts have execute permissions, then:
```
./runtests.sh
```
for running all tests or:
```
./encodings.sh ./tests/P{n}/test{n} n
```
for comparing the four encodings using a specific test, for example:
```
./encodings.sh ./tests/P14/test14 14
```
or
```
./systems.sh ./tests/P{n}/test{n} n
```
for comparing encodings 3 and 4 with psmodels and lpod2asprin using a specific test