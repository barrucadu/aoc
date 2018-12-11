#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.cabal-install haskellPackages.ghc linuxPackages.perf

set -e

cd solutions
cabal new-build >/dev/null

for f in dist-newstyle/build/*/*/*/x/*/build/*/Day*Part*; do
  if [[ -f $f ]] && [[ -x $f ]]; then
    echo "== $(basename $f)"
    perf stat -r50 $f >/dev/null
  fi
done
