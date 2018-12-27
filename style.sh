#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskellPackages.stylish-haskell

set -e

find . -name '*.hs' -exec stylish-haskell -i {} \;
