TOPTARGETS := all clean

EXAMPLES := $(wildcard examples/*/.)

all: build $(EXAMPLES)

build: output

output: .spago node_modules
	npx spago build

.spago: node_modules
	npx spago install

node_modules:
	npm i --no-save spago purescript bower pulp

clean: $(EXAMPLES)
	rm -rf node_modules bower_components .spago output

$(EXAMPLES):
	cd $@ && $(MAKE) $(MAKECMDGOALS)

.PHONY: build clean $(TOPTARGETS) $(EXAMPLES)