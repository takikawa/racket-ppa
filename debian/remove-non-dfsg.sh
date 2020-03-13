#!/bin/bash

# run this script on the _upstream_ branch

( while  read -r name ; do
    git rm -r $name
done ) <<EOF
collects/tests/racket/benchmarks/common/psyntax-input.txt
collects/tests/xml/clark-tests
collects/tests/racket/benchmarks/common/maze.sch
collects/tests/racket/benchmarks/common/maze2.sch
collects/tests/racket/benchmarks/common/typed/maze2.rktl
collects/tests/racket/testing.rktl
collects/tests/utils/mz-testing.rkt
EOF
