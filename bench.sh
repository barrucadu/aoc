#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.bench haskellPackages.cabal-install haskellPackages.ghc

cd solutions
cabal new-build >/dev/null

for f in dist-newstyle/build/*/*/*/x/*/build/*/Day*Part*; do
  if [[ -f $f ]] && [[ -x $f ]]; then
    echo "== $(basename $f)"
    bench $f | sed 1d
  fi
done
