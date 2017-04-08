#! /usr/bin/env bash

ghc --version
cabal update
cabal install sha-streams
cd scp-streams
cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test
#cabal install
#cabal sdist
