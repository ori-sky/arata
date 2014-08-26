GHC=ghc
CFLAGS=-W -O2 -isrc
EXECUTABLE=arata

.PHONY: all
all: build

.PHONY: build
build:
	$(GHC) $(CFLAGS) src/Main -o $(EXECUTABLE)

.PHONY: clean
clean:
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv
	rm -fv $(EXECUTABLE)
