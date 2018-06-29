all: lifechart

lifechart: src/*.elm
	elm-make src/Lifechart.elm --yes --warn --output public/lifechart.js
	uglifyjs --mangle --screw-ie8 --output public/lifechart.min.js -- public/lifechart.js
	rm public/lifechart.js

watch: src/*.elm
	elm-test --watch

clean:
	$(RM) -r public/lifechart*.js elm-stuff/

.PHONY: watch clean
