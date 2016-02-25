#!/bin/sh

cp scripts/* $RISCV/bin
cd axe/src
./make.sh
cp axe $RISCV/bin
cp axe-shrink.py $RISCV/bin
