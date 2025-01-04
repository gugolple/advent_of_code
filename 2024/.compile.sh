#!/bin/bash
source /home/ghc/.ghcup/env
cd $SRC_DIR
cd $(dirname $TGT)
cp * /tmp
pushd /tmp
filename=$(basename -- "$TGT")
filename_nxt="${filename%.*}"
ghc -o $filename_nxt $filename
./$filename_nxt
