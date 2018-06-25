#!/bin/bash

DUMP_PATH=`pwd`

STATS_DB=coreutils-arm.stats
SKETCH_DB=coreutils-arm-sketch.db
#STATS_DB=coreutils-x86.stats
#SKETCH_DB=coreutils-x86-sketch.db


PATTERN=coreutils/arm/* # ARM
#PATTERN=coreutils/x86/* # x86

#time ls ${PATTERN} | head -n 1 |\
time ls ${PATTERN} |\
  xargs -L 1 -I % bash -c \
  "bap % --loader=ida --rooter=ida --symbolizer=ida \
  -lmain --main\
  --main-miner \
  --main-bil-sz=8 \
  --main-with-miner-stats=${DUMP_PATH}/${STATS_DB} \
  --main-with-miner-db=${DUMP_PATH}/${SKETCH_DB}\
  --main-pp-miner-db \
  --main-pp-miner-stats"

echo 'Remember to delete the db if you rerun this script!'
