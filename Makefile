default: ag
	cabal configure
	cabal build

dist:
	cabal configure
	cabal sdist


install: ag
	cabal install

ag: src/Base.ag
	uuagc -dcfws --self --genlinepragmas src/Base.ag

doc: ag
	cabal configure
	cabal haddock --executables

clean:
	-rm src/Parser/Base.hs
	cabal clean

lint:
	hlint \
		src/CLIFuncs.hs \
	
#churn:
#	hg churn --aliases authormap

.PHONY: dist
