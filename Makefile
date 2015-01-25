.PHONY: all
all: install

.PHONY: sandbox
sandbox:
	cabal sandbox init

.PHONY: deps
deps: sandbox
	cabal install --only-dependencies

.PHONY: build
build: deps
	cabal build

.PHONY: install
install: build
	cp -v dist/build/arata/arata ./

.PHONY: run
run: install
	cabal exec ./arata

.PHONY: clean
clean:
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv
	rm -rfv dist
	rm -fv arata

.PHONY: clean-plugins
clean-plugins:
	find plugins -name '*.o' -print0 | xargs -0 rm -fv
	find plugins -name '*.hi' -print0 | xargs -0 rm -fv
