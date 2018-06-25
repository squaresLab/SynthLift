#!/bin/bash

LIFTER_DB=mips-lifter.db
#BINARY=../benchmarks/mips/snoobdir/snoob
#PATTERN=../benchmarks/mips/coreutils/bin/rmdir
#PATTERN=/tmp/DGL/_dgl5500_firmware_101.bin.extracted/squashfs-root/www/cgi/ssi
PATTERN=unused-mips

time ls ${PATTERN} |\
  xargs -L 1 -I % bash -c \
  "bap % \
    --loader=ida --rooter=ida --symbolizer=ida \
    -lmain --main \
    --main-lifter \
    --main-lifter-db=${LIFTER_DB} \
    --warn-unused"
