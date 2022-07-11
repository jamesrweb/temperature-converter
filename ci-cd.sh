#!/usr/bin/env bash

set -e

outputLocation="dist"
input="src/Main.elm"
main="$outputLocation/elm.js"
minified="$outputLocation/elm.min.js"

if [ -d "$outputLocation" ]; then
	rm -rf dist
fi

mkdir "$outputLocation"

# CI
pnpx elm-format . --validate
pnpx elm-review .
pnpx elm-test

# CD
pnpx elm make --optimize --output="$main" "$input"
pnpx esbuild "$main" --target="es5" --outfile="$minified" --sourcemap="external" --minify

echo "Initial size: $(cat $main | wc -c) bytes ($main)"
echo "Minified size: $(cat $minified | wc -c) bytes ($minified)"
echo "Gzipped size: $(cat $minified | gzip -c | wc -c) bytes"
