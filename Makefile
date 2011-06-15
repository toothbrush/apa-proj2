default: ag
	cabal configure
	cabal build

dist:
	cabal configure
	cabal sdist


install: ag
	cabal install

ag: src/Parser/Base.ag
	uuagc -dcfws --self --genlinepragmas src/Parser/Base.ag

doc: ag
	cabal configure
	cabal haddock --executables

clean:
	# -rm src/JSMonotoneFWAG.hs src/SoftTypingAG.hs src/ControlFlowAG.hs
	cabal clean

lint:
	hlint \
		src/CLIFuncs.hs \
	
#churn:
#	hg churn --aliases authormap

.PHONY: dist
