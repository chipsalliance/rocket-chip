#!/bin/bash

set -ex

export RISCV="$(pwd)/riscv-prefix"
export PATH="$RISCV/bin:$PATH"
export TOP="$(pwd)"

rm -rf $RISCV
git submodule update --init --recursive

cd "$TOP"/riscv-tools
./build.sh

cd "$TOP"
make -C emulator
make -C emulator run-asm-tests
make -C emulator run-bmarks-test
