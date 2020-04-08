parent_sources := $$(npx spago -x ../../spago.dhall sources | sed 's/^/..\/..\//' | tr '\n' ' ')

all: output node_modules
	npx purs bundle -m Main --main Main output/*/*.js > output/bundle.js
	npx browserify output/bundle.js -o html/index.js

output: node_modules
	set -o noglob && \
	  npx purs compile \
		  src/**/*.purs \
		  $(parent_sources)

node_modules:
	npm i --no-save browserify react react-dom

clean:
	rm -rf node_modules output

.PHONY: all output clean