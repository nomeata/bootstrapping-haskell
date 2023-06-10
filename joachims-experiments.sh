#!/usr/bin/env bash

test -f ghc-4.08.2-src.tar.bz2 || wget -c https://downloads.haskell.org/~ghc/4.08.2/ghc-4.08.2-src.tar.bz2
test -d ghc-4.08.2/ || tar xjf ghc-4.08.2-src.tar.bz2

mkdir -p generated
touch generated/HsVersions.h
cd ghc-4.08.2
happy ghc/compiler/parser/Parser.y -o generated/Parser.hs
hugs -F"cpphs-hugs --noline -I../includes/" '-Pghc/compiler/*:{Hugs}/oldlib:../generated/:' ghc/compiler/main/Main.lhs
