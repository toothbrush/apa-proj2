CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/CCO/HM/AG.hs : src/CCO/HM/AG.ag src/CCO/HM/AG/Base.ag src/CCO/HM/AG/Infer.ag
	uuagc -Hdcfws --self -P src/CCO/HM src/CCO/HM/AG.ag

src/CCO/SystemF/AG.hs : src/CCO/SystemF/AG.ag src/CCO/SystemF/AG/Base.ag \
		src/CCO/SystemF/AG/Printing.ag
	uuagc -Hdcfws --self -P src/CCO/SystemF src/CCO/SystemF/AG.ag

haskell : src/CCO/HM/AG.hs src/CCO/SystemF/AG.hs
	chmod a+x test.sh
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

documentation: latex-doc/main.tex
	pdflatex -output-directory=latex-doc latex-doc/main.tex 

clean : 
	-rm src/CCO/HM/AG.hs
	-rm src/CCO/SystemF/AG.hs
	-rm latex-doc/main.pdf
	-rm latex-doc/main.aux
	-rm latex-doc/main.log
	-rm latex-doc/main.toc
	cabal clean

.PHONY : haskell clean
