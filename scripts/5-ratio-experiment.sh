#!/bin/bash

LIFTER_DB=mips-lifter.db
PATTERN=coreutils/mips/*

time ls ${PATTERN} |\
  xargs -L 1 -I % bash -c \
  "bap % \
    --loader=ida --rooter=ida --symbolizer=ida \
    -lmain --main \
    --main-lifter \
    --main-lifter-db=${LIFTER_DB}"

#--no-cache
