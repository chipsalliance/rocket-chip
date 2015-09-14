#! /bin/bash
#
# See LICENSE for license details.

# Script to setup submodules, build rocket-chip, and run asm tests, and optionally run torture

set -ex

echo "Starting Rocket-chip regression test"
if [ $# -lt 1 ]
then
  echo "Usage: ./regression.sh config [torture_config] [torture_output_dir]"
  exit
fi

git submodule update --init --recursive riscv-tools
export RISCV="$(pwd)/install"; export PATH=$PATH:$RISCV/bin
cd riscv-tools; ./build.sh; cd ..
git submodule update --init 
git submodule status --recursive
cd emulator; make CONFIG=$1; make CONFIG=$1 run-asm-tests; cd ..
if [ $# -ge 2 ]
then
  cd emulator; make CONFIG=$1 debug; cd ..
  git clone git@github.com:ucb-bar/riscv-torture.git
  cd riscv-torture; git submodule update --init;
  if [ $# -eq 3 ]
  then
    make cnight RTL_CONFIG=$1 OPTIONS="-C $2 -p $3 -m 30 -t 10"
  else
    make cnight RTL_CONFIG=$1 OPTIONS="-C $2 -m 30 -t 10"
  fi
fi

