#!/bin/sh

if [ "$PLTCOLLECTS" = '' ] ; then
  PLTCOLLECTS=:/usr/local/lib/plt/collects
  export PLTCOLLECTS
fi

/usr/lib/plt/bin/mred.bin "$@"
