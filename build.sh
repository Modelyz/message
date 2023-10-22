#!/bin/bash

OPT_DEVEL='--ghc-options="-Wall"'
OPT_OPTIMIZE=' -O2 --ghc-options="-Wall" --enable-library-stripping'

pushd $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

[ -f ./dist-newstyle/cache/plan.json ] && which cabal-cache && cabal-cache sync-from-archive --threads 4 --archive-uri s3://cabal-store.prelab.fr --host-name-override=s3.fr-par.scw.cloud --host-port-override=443 --host-ssl-override=True --region fr-par

if [ "$1" == "-o" ]; then
    # TODO stripping does not work
    cabal build $OPT_OPTIMIZE
else
    cabal build $OPT_DEVEL
fi

which cabal-cache && cabal-cache sync-to-archive --threads 4 --archive-uri s3://cabal-store.prelab.fr --host-name-override=s3.fr-par.scw.cloud --host-port-override=443 --host-ssl-override=True --region fr-par

popd
