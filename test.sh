#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.cabal-install haskellPackages.ghc

set -e

year="${1:-}"

if [[ ! -d "$year/solutions" ]]; then
  echo "usage: $0 <year>"
  exit 1
fi

cd "$year/solutions"
cabal new-build

ret=0

for f in dist-newstyle/build/*/*/*/x/*/build/*/Day*Part*; do
  if [[ -f $f ]] && [[ -x $f ]]; then
    exe=$(basename $f)
    if [[ -e ../outputs/$exe ]]; then
      expected=$(cat ../outputs/$exe)
    else
      expected=""
    fi
    actual=$($f)
    if [[ "$expected" == "$actual" ]]; then
      echo -en "\033[0;32m.\033[0m"
    else
      echo
      echo -e "\033[0;31m== $exe\033[0m"
      echo -e "\033[0;31mexpected:\033[0m"
      echo -e "\n$expected\n" | sed 's/^/    /'
      echo -e "\033[0;31mbut found:\033[0m"
      echo -e "\n$actual\n" | sed 's/^/    /'

      if [[ -n "${ACCEPT:-}" ]]; then
        echo -e "\033[0;32maccepting new solution\033[0m"
        echo "$actual" > "../outputs/${exe}"
      else
        ret=1
      fi
    fi
  fi
done

echo

exit $ret
