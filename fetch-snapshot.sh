#!/bin/bash

# this script fetches a pre-release snapshot from racket upstream
# the hash is used to try to match the tarball up with git history.

hash=$(wget -O- http://pre.racket-lang.org/build-log.txt| sed -n '0,/Updating/ s/^Updating.*[.][.]//p')

stamp=$(wget -O- http://pre.racket-lang.org/stamp)
date=${stamp%%???? *}
version=${stamp##* }

name="racket_$version+$date~$hash.orig.tar.gz"

echo fetching $name
wget -O "$name" http://pre.racket-lang.org/pre-installers/plt-src-unix.tgz
