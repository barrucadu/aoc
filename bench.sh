#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.cabal-install haskellPackages.ghc linuxPackages.perf

set -e

year="${1:-}"

if [[ ! -d "$year/solutions" ]]; then
  echo "usage: $0 <year>"
  exit 1
fi

cd "$year/solutions"
cabal new-build >/dev/null

cat > ../README.md <<EOF
Advent of Code ${year}
===================

https://adventofcode.com/${year}

Benchmark results
-----------------

EOF

for f in dist-newstyle/build/*/*/*/x/*/build/*/Day*Part*; do
  if [[ -f $f ]] && [[ -x $f ]]; then
    echo "### $(basename $f)" | tee -a ../README.md
    echo '```' >> ../README.md
    perf stat -r50 $f 2>&1 >/dev/null | tee -a ../README.md
    echo '```' >> ../README.md
  fi
done
