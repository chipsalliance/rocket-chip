#!/bin/bash -euxo pipefail

BASE_DIR=$(echo "$(cd "$(dirname "$0")/.." && pwd)")

configs=(
  DefaultSmallConfig
  DualCoreConfig
  #EightChannelConfig
  #RoccExampleConfig
  #Edge128BitConfig
  #OctoChannelBenchmarkConfig
  TinyConfig
  MemPortOnlyConfig
  #MMIOPortOnlyConfig
)

MAXPROCS=4
PACKAGE=freechips.rocketchip.system
CMD="RISCV=$RISCV STABILIZE=0 CONFIG=% make -C $BASE_DIR/emulator verilog"

# build just the DefautlConfig first one first so the other configs don't duplicate work
echo "$PACKAGE.DefaultConfig" \
| xargs -I % sh -c "$CMD"


printf 'freechips.rocketchip.system.%s\n' "${configs[@]}" \
| xargs -P $MAXPROCS -I % sh -c "$CMD"


ORIGINAL_FILE=$BASE_DIR/stabilized/original.v
COMPARE_DIR=$BASE_DIR/stabilized
GENERATED_SRC_DIR=$BASE_DIR/emulator/generated-src
cd $BASE_DIR
cp $GENERATED_SRC_DIR/$PACKAGE.DefaultConfig.stabilized.v $ORIGINAL_FILE
cp $GENERATED_SRC_DIR/*.stabilized.v $COMPARE_DIR/
$BASE_DIR/scripts/compare_names.py --short --original-file $ORIGINAL_FILE --modified-files $COMPARE_DIR/*.stabilized.v > $COMPARE_DIR/results.txt
