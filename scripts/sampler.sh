#!/bin/bash -x

BASE_DIR=$(echo "$(cd "$(dirname "$0")/.." && pwd)")

configs=(
  DefaultSmallConfig
  DualCoreConfig
  EightChannelConfig
  RoccExampleConfig
  Edge128BitConfig
  OctoChannelBenchmarkConfig
  TinyConfig
  MemPortOnlyConfig
  MMIOPortOnlyConfig
)

RISCV=$RISCV STABILIZE=0 make -C $BASE_DIR/emulator $BASE_DIR/firrtl/utils/bin/firrtl/firrtl.jar

maxprocs=4
printf 'freechips.rocketchip.system.%s\n' "${configs[@]}" \
| xargs -P $maxprocs -I % sh -c "RISCV=$RISCV STABILIZE=0 CONFIG=% make -C $BASE_DIR/emulator verilog"
