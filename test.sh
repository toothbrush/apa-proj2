#!/bin/sh
# 
# Author: Paul van der Walt
# 
# This little file is basically an alias to the long pipe you would
# otherwise have to type in. Invoke like:
#
#   $ ./test.sh examples/identity.hm
#

cat $1 | dist/build/parse-hm/parse-hm | dist/build/hm2systemf/hm2systemf | dist/build/pp-systemf/pp-systemf
