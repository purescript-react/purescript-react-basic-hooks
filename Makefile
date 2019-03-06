all: build examples

build: bower_components node_modules
	npx pulp build

examples: bower_components node_modules
	find examples -maxdepth 2 -type f -iname makefile -execdir make \;

bower_components: node_modules
	npx bower --allow-root install

node_modules:
	npm i --no-save bower pulp purescript

.PHONY: build examples
