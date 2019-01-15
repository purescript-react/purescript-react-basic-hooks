all: build examples

build: bower_components
	pulp build

examples: bower_components
	find examples -maxdepth 2 -type f -iname makefile -execdir make \;

bower_components:
	bower install

.PHONY: build examples
