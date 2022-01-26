#!/bin/bash

if [ $# -ne 1 ] ; then
    echo "Usage: ./run.sh <relative module path>"
    echo "Example: ./run.sh src/Foo/A.ml"
    exit 0
fi

MODULE=$(basename -s .ml $1)

if [ "$MODULE" = "$1" ] ; then
    echo "$1 is not a .ml file"
    exit 0
fi

FOLDER=$(dirname $1)

PATH=_build/$FOLDER/$MODULE.native

if [ ! -f "$PATH" ] ; then
    echo "Native code not found, did you compile?"
    exit 0
fi

./$PATH