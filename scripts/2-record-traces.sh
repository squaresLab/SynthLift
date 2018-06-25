#!/bin/bash

DUMP_PATH=`pwd`

DUMP_IO_DB=${DUMP_PATH}/mips-io.db
MIPS_TRACE=../benchmarks/mips-frames/snoob.frames
#MIPS_TRACE=../benchmarks/mips-frames/clp2.frames
#MIPS_TRACE=../benchmarks/mips-frames/parity.frames
#MIPS_TRACE=../benchmarks/mips-frames/pop.frames
#MIPS_TRACE=../benchmarks/mips-frames/sparse.frames

bap /bin/echo \
  -lmain --main \
  --main-tracer \
  --main-with-tracer-db=${DUMP_IO_DB} \
  --main-trace=${MIPS_TRACE}
