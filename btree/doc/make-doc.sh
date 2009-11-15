#!/bin/sh
DOCDIR=`dirname $0`
texi2pdf --clean $DOCDIR/btree.texinfo

