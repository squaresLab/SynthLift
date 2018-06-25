#!/bin/bash

DUMP_PATH=`pwd`

DUMP_LIFTER_DB=${DUMP_PATH}/mips-lifter.db
IO_DB=mips-io.db
SKETCH_DB=coreutils-arm-sketch.db
#SKETCH_DB=coreutils-i386-sketch.db

bap /bin/echo \
  -lmain --main \
  --main-synthesizer \
  --main-synthesizer-io-db=${IO_DB} \
  --main-synthesizer-sketch-db=${SKETCH_DB} \
  --main-with-synthesizer-db=${DUMP_LIFTER_DB}
