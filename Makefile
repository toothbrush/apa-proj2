default: ag
	cabal configure
	cabal build
	chmod a+x test.sh

dist:
	cabal configure
	cabal sdist


install: ag
	cabal install

ag: src/Base.ag \
	src/Infer/Infer.ag \
	src/Infer/Typed.ag
	uuagc -dcfws --self --genlinepragmas src/Base.ag
	uuagc -dcfws --self --genlinepragmas src/Infer/Typed.ag

doc: ag
	cabal configure
	cabal haddock --executables

clean:
	-rm src/Base.hs \
		src/Infer/Infer.hs
	cabal clean

lint:
	hlint \
		src/Parser.hs \
		src/TreeInstances.hs
	
#churn:
#	hg churn --aliases authormap

.PHONY: dist
