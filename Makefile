SHELL := /bin/bash

all: test configure
	cabal build
	cp dist/build/SpaceLighting/SpaceLighting .
	cp dist/build/SpaceLighting/SpaceLighting SpaceLightingAsg5
test:
	if [[ "$$(which cabal)" == "" ]]; then echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1; fi
configure:
	cabal update
	cabal install cabal
	cabal install --only-dependencies
	cabal configure
clean:
	cabal clean
	- rm -fr SpaceLighting SpaceLightingAsg5 dist
	