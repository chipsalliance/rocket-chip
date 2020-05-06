#!/bin/bash -x

BASE_DIR=$(dirname $0)

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
maxprocs=4
printf '%s\n' "${configs[@]}" \
| xargs -P $maxprocs -I % sh -c "RISCV=$RISCV STABILIZE=0 CONFIG=% make -C $BASE_DIR/emulator verilog"
