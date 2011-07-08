# README file for the second assignment of Automated Program Analysis

Control Flow Analysis

Block 4 2010/2011

Authors: Paul van der Walt (3120805), Ruben de Gooijer (3508617), Jurriën
Stutterheim (3555003)

---------------------------------------

## System Requirements

Tested on Mac OS X 10.6.7 and 10.7.0 with GHC 7.0.3 and Haskell Platform
2011.


### Additional requirements

Building the package requires the latest version of the UU Attribute
Grammar System. Issue a

> cabal update && cabal install uuagc

before attempting to install the program.


## Compilation

Simply issue

> make

to build the program


## Execution

The program expects a file to be piped to it:

> cat examples/poisoning.hm | ./dist/build/cfa/cfa

Several examples are available in the examples directory.
