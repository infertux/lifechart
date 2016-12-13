#!/bin/bash -eux

elm-make Lifechart.elm --warn --output lifechart.js
uglifyjs --mangle --screw-ie8 --output lifechart.min.js -- lifechart.js
