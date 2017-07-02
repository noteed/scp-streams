#! /usr/bin/env bash

ghc --version
#cabal update
ls
cabal install sha-streams/sha-streams.cabal
cd scp-streams
cabal configure --enable-tests || exit 1
cabal build || exit 1
cabal test
#cabal install
#cabal sdist
