#!/bin/bash -eux

cd $(dirname $0)

elm-make Lifechart.elm --warn --output lifechart.js

uglifyjs --mangle --screw-ie8 --output lifechart.min.js -- lifechart.js

[ -d dist ] || mkdir dist
cp -v index.html lifechart.min.js dist/
