#!/bin/bash

OPT_DEVEL='--ghc-options="-Wall"'
OPT_OPTIMIZE=' -O2 --ghc-options="-Wall" --enable-library-stripping'

set -xe

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

which cabal-cache && cabal-cache sync-from-archive --archive-uri ~/.cabal/archive

if [ "$1" == "-o" ]; then
    # TODO stripping does not work
    cabal build $OPT_OPTIMIZE
else
    cabal build $OPT_DEVEL
fi

which cabal-cache && cabal-cache sync-to-archive --archive-uri ~/.cabal/archive

popd
