I've been trying to get GHC 3.02 building with Hugs, because it was the
earliest one I could find the source for.

# Finding the source

[This page](https://www.haskell.org/ghc/download_ghc_302.html) links to [a
tarball](https://downloads.haskell.org/~ghc/3.02/ghc-3.02-src.tar.gz)

# Dealing with LibSystem

As far as I can tell, the source expects the compiler to implement a _draft_ of
the standard adding monadic IO. The draft differs from the final version (used
by Hugs) in that the module where `IO` is defined is `System`, not `LibSystem`.
I changed all the imports accordingly.

# Recursive modules

This is a problem

# Running hugs

The following command runs Hugs on the source for GHC 3.02

```bash
hugs -F"cpphs-hugs --noline -Ighc-3.02/ghc/compiler/ " -Pghc-3.02/ghc/compiler/*: ghc-3.02/ghc/compiler/main/Main.lhs
```
