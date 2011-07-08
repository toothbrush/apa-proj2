CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all: haskell

ag: src/APA2/AG.hs

src/APA2/AG.hs: src/APA2/AG.ag src/APA2/AG/DataTypes.ag src/APA2/AG/Infer.ag src/APA2/AG/FreeVars.ag src/APA2/AG/FreeTyVars.ag src/APA2/AG/FreeAnnVars.ag src/APA2/AG/CollectBinders.ag
	uuagc -dcfws --self --genlinepragmas -P src/APA2 src/APA2/AG.ag

haskell: src/APA2/AG.hs
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

doc: doc/main.tex
	pdflatex -output-directory=doc doc/main.tex

lint:
	hlint src/*.hs

clean : 
	-rm src/APA2/AG.hs
	-rm doc/main.pdf
	-rm doc/main.aux
	-rm doc/main.log
	-rm doc/main.toc
	cabal clean

.PHONY : haskell clean doc
