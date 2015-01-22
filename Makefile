GHC=ghc
CFLAGS=-Wall -Werror -fno-warn-missing-signatures -fno-warn-unused-do-bind -O2 -isrc
EXECUTABLE=arata

.PHONY: all
all: build

.PHONY: build
build:
	$(GHC) $(CFLAGS) src/Main -o $(EXECUTABLE)

.PHONY: install
install: build
	mkdir -p dist
	cp -v arata arata.conf.example dist/

.PHONY: clean
clean:
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv
	rm -fv $(EXECUTABLE)

.PHONY: register
register:
