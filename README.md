# README file for the second assignment of Automated Program Analysis

Control Flow Analysis

Block 4 2010/2011

Authors: Paul van der Walt (3120805), Ruben de Gooijer (3508617), Jurriën
Stutterheim (3555003)

## System Requirements

Tested on Mac OS X 10.6.7 and 10.7.0 with GHC 7.0.3 and Haskell Platform
2011.


### Additional requirements

Building the package requires the latest version of the UU Attribute
Grammar System. Issue a

> cabal update && cabal install uuagc

before attempting to install the program.

Please note that the program makes heavy use of Unicode, so using a
Unicode-compatible terminal emulator is required. Most modern systems shouldn't
have a problem with this, though.


## Compilation

Simply issue

> make

to build the program


## Execution

The program expects a file to be piped to it:

> cat examples/poisoning.hm | ./dist/build/cfa/cfa

Several examples are available in the examples directory.


### Understanding the output

When running the program, as stated in the section above, depending on
the size of the input code, the output can be quite overwhelming. It could be
advisable to view the output with a pager such as `less`.

The first thing one sees is the program that was input, for reference, pretty-
printed. Next follows the principal type of the entire program, including
it's annotation (in terms of program points). Next follows the lookup table
of program points, so one can get an idea of what the program points mean.
Finally, all sub-expressions are printed. Bear in mind that context is
important: some variable x which appears twice in this list is a different
x, for example bound in two different alternatives of a "case" statement.

### Extras

The program can also do a couple of other things. Type

> ./dist/build/cfa/cfa help

to get an overview. The most interesting extra is the 'debug'
mode, where all intermediate information is printed. Having said
"most intersting", one should however note that this is just relative,
and that it is therefore still rather uninteresting.
