#!/bin/bash

LIFTER_DB=mips-lifter.db
BINARY=unused-mips
#BINARY=unused-mips-no-bug
#BINARY=../benchmarks/mips/snoobdir/snoob
#BINARY=../benchmarks/mips/openssl/usr/bin/openssl

bap $BINARY \
  --loader=ida --rooter=ida --symbolizer=ida \
  -lmain --main \
  --main-lifter \
  --main-lifter-db=${LIFTER_DB} \
  --print-with-colors=true \
  --print-bir-attr={background,foreground} \
  --print-bir-attr=comment \
  -dbir --warn-unused -dbir
