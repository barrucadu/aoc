#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.bench haskellPackages.cabal-install haskellPackages.ghc

cd solutions
cabal new-build >/dev/null

for f in Day*.hs; do
  exename=`echo $f | sed 's/.hs$//'`
  exe=dist-newstyle/build/*/*/*/x/*/build/*/$exename
  echo "== $exename"
  bench $exe | sed 1d
done
