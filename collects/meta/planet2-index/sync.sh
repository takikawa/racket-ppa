#!/bin/sh

for i in planet2 tests/planet2 meta/planet2-index ; do
    rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../$i/ plt-etc:local/galaxy/$i/
done

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/planet2-index/$i/root/ $i/root/
done

rsync -a --progress -h --delete --exclude root --exclude compiled --exclude doc ../../planet2/ plt-etc:local/plt/collects/$i/
