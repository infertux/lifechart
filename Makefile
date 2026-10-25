all: lifechart

lifechart: src/*.elm
	elm make src/Lifechart.elm --optimize --output public/lifechart.js
	uglifyjs public/lifechart.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/lifechart.min.js
	rm public/lifechart.js

watch: src/*.elm
	elm-test --watch

clean:
	$(RM) -r public/lifechart*.js elm-stuff/

.PHONY: watch clean
