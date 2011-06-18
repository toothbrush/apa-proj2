CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all: haskell

src/APA2/AG.hs: src/APA2/AG.ag src/APA2/AG/Base.ag src/APA2/AG/Infer.ag
	uuagc -dcfws --self --genlinepragmas -P src/APA2 src/APA2/AG.ag

haskell: src/APA2/AG.hs
	chmod a+x test.sh
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

documentation: latex-doc/main.tex
	pdflatex -output-directory=latex-doc latex-doc/main.tex 

clean : 
	-rm src/APA2/AG.hs
	-rm latex-doc/main.pdf
	-rm latex-doc/main.aux
	-rm latex-doc/main.log
	-rm latex-doc/main.toc
	cabal clean

.PHONY : haskell clean
