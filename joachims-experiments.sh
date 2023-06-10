#!/usr/bin/env bash

set -e

test -f ghc-4.08.2-src.tar.bz2 || wget -c https://downloads.haskell.org/~ghc/4.08.2/ghc-4.08.2-src.tar.bz2
test -d ghc-4.08.2/ || tar xjf ghc-4.08.2-src.tar.bz2

mkdir -p includes generated
touch includes/HsVersions.h
cd ghc-4.08.2
happy ghc/compiler/parser/Parser.y -o ../generated/Parser.hs
echo :quit | hugs -F"cpphs-hugs --noline -I../includes/" '-Pghc/compiler/*:{Hugs}/oldlib:../generated/:../shims:' ghc/compiler/utils/BitSet.lhs
echo :quit | hugs -F"cpphs-hugs --noline -I../includes/" '-Phslibs/lang:ghc/compiler/*:{Hugs}/oldlib:{Hugs}/oldlib:../generated/:../shims:' ghc/compiler/utils/UniqSet.lhs
