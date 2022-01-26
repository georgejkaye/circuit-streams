#!/bin/bash

if [ $# -ne 1 ] ; then
    echo "Usage: ./compile.sh <relative module path>"
    echo "Example: ./compile.sh src/Foo/A.ml"
    exit 0
fi

MODULE=$(basename -s .ml $1)

if [ "$MODULE" = "$1" ] ; then
    echo "$1 is not a .ml file"
    exit 0
fi

FOLDER=$(dirname $1)

if ! hash ocamlbuild 2>/dev/null ; then
    echo "ocamlbuild not installed, have you run opam install ocamlbuild?"
    exit 0
fi

if ! hash ocamlfind 2>/dev/null ; then
    echo "ocamlfind not installed, have you run opam install ocamlfind?"
    exit 0
fi

if ! hash pcregrep 2>/dev/null ; then
    echo "pcregrep not installed"
    exit 0
fi

# Grab packages from .merlin
if [ -f ".merlin" ] ; then
    PKGS=$(pcregrep -o1 'PKG ([a-z_]*)' .merlin)
fi

for pkg in $PKGS ; do
    if [ -z "$PACKAGES" ] ; then
        PACKAGES="$pkg"
    else
        PACKAGES="$PACKAGES,$pkg"
    fi
done

if [ ! -z "$PACKAGES" ] ; then
    PKG_STRING="-pkgs $PACKAGES"
fi

for dir in _build/*/ ; do
    if [ -z "$INCLUDES" ] ; then
        INCLUDES="$dir"
    else
        INCLUDES="$INCLUDES,$dir"
    fi
done

OCAMLBUILD="ocamlbuild -use-ocamlfind $PKG_STRING $FOLDER/$MODULE.native"

echo "Running $OCAMLBUILD"
$OCAMLBUILD

if [ -f "$MODULE.native" ] ; then
    echo "$FOLDER/$MODULE compiled successfully!"
    rm $MODULE.native
else 
    echo "$FOLDER/$MODULE compiled unsuccessfully."
fi
