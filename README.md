# lpod2asp
LPOD to ASP transpiler written in Haskell. It translates LPODs to ASP using one of four availiable encodings and then `clingo` and `asprin` can be used to find the most preferred answer sets.


## Requirements

1. [`GHC`](https://www.haskell.org/ghc/)
2. [`Alex`](https://haskell-alex.readthedocs.io/en/latest/about.html)
3. [`Happy`](https://haskell-happy.readthedocs.io/en/latest/introduction.html)
4. [`Make`](https://www.gnu.org/software/make/)
5. [`asprin`](https://github.com/potassco/asprin) and `clingo` (should be installed as dependency of `asprin`)


## Setup

```
git clone https://github.com/grnkl/lpod2asp.git
cd lpod2asp
make
```
Then, the executables can be found in the build directory.

## Usage

The easiest way to use solve an LPOD is using the `lpod2asp.sh` script. Make sure it has execute permissions, then:

```bash
./lpod2asp.sh example.lpod [-enc i]
```
where i=1,2,3,4 is the encoding to be used (default = 4)

## Related projects

1. [LPOD solver](https://github.com/acharal/lpod)
2. [psmodels](http://www.tcs.hut.fi/Software/smodels/priority/)
3. [lpod.pl](https://www.dc.fi.udc.es/~cabalar/lpod/)
4. [lpod2asprin](https://github.com/zhunyoung/lpod2asprin)
