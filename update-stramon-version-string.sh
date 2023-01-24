#!/bin/bash

path="bin/version_string.ml"

ver=$(cat CHANGELOG.md \
  | grep '## Stramon' \
  | head -n 1 \
  | sed -n 's/^## Stramon\s*\(\S*\)$/\1/p')

echo "Detected version for Stramon:" $ver

echo "Writing to" $path

echo "let s = "\"$ver\" > $path
