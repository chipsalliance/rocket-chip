#!/bin/bash

# This file was originally written by Matthew Naylor, University of
# Cambridge.
#
# This software was partly developed by the University of Cambridge
# Computer Laboratory under DARPA/AFRL contract FA8750-10-C-0237
# ("CTSRD"), as part of the DARPA CRASH research programme.
# 
# This software was partly developed by the University of Cambridge
# Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
# ("MRC2"), as part of the DARPA MRC research programme.
# 
# This software was partly developed by the University of Cambridge
# Computer Laboratory as part of the Rigorous Engineering of
# Mainstream Systems (REMS) project, funded by EPSRC grant
# EP/K008528/1. 

# This script can be used to test the memory consistency of rocket
# chip in simulation when compiled with the 'TraceGenConfig'
# configuation.

###############################################################################

# Parameters
# ==========

# Arguments are taken from environment variables where available.
# Elsewhere, defaults values are chosen.

START_SEED=${START_SEED-0}
NUM_TESTS=${NUM_TESTS-100}
EMU=${EMU-emulator-groundtest-TraceGenConfig}
TRACE_GEN=${TRACE_GEN-tracegen.py}
TO_AXE=${TO_AXE-toaxe.py}
AXE=${AXE-axe}
MODEL=${MODEL-WMO}
LOG_DIR=${LOG_DIR-tracegen-log}
TRACE_STATS=${TRACE_STATS-tracestats.py}

###############################################################################

# Inferred parameters
# ===================

END_SEED=`expr \( $START_SEED + $NUM_TESTS \) - 1`
LOG=$LOG_DIR
PATH=$PATH:.

# Sanity check
# ============

if [ ! `command -v $EMU` ]; then
  echo Can\'t find emulator: \'$EMU\'
  exit -1
fi

if [ ! `command -v $TO_AXE` ]; then
  echo Please add \'toaxe.py\' to your PATH
  exit -1
fi

if [ ! `command -v $TRACE_GEN` ]; then
  echo Please add \'tracegen.py\' to your PATH
  exit -1
fi

if [ ! `command -v $AXE` ]; then
  echo Please add \'axe\' to your PATH
  exit -1
fi

if [ ! `command -v $TRACE_STATS` ]; then
  echo Please add \'tracestats.py\' to your PATH
  exit -1
fi

if [ "$MODEL" != SC  -a \
     "$MODEL" != TSO -a \
     "$MODEL" != PSO -a \
     "$MODEL" != WMO -a \
     "$MODEL" != POW ]; then
  echo Unknown consistency model \'$MODEL\'
  exit -1
fi

# Setup log directory
# ===================

if [ ! -d $LOG ]; then
  echo Creating log directory: $LOG
  mkdir $LOG
fi

rm -f $LOG/errors.txt
rm -f $LOG/stats.txt

# Test loop
# =========

echo Testing against $MODEL model:

for (( I = $START_SEED; I <= $END_SEED; I++ )); do
  SPACE=`expr $I \% 10`
  if [ $SPACE -eq 0 ]; then
    echo -n " "
  fi

  NEWLINE=`expr $I \% 50`
  if [ $NEWLINE -eq 0 ]; then
    printf "\n%8i: " $I
  fi

  # Generate trace
  $TRACE_GEN $EMU $I > $LOG/trace.txt
  if [ ! $? -eq 0 ]; then
    echo -e "\n\nError: emulator returned non-zero exit code"
    echo See $LOG/trace.txt for details
    exit -1
  fi

  # Convert to axe format
  $TO_AXE $LOG/trace.txt $LOG/stats.txt 2>> $LOG/errors.txt > $LOG/trace.axe
  if [ ! $? -eq 0 ]; then
    echo -e "\n\nError during trace generation with seed $I"
    echo "See $LOG/errors.txt"
    exit -1
  else
    # Check trace
    OUTCOME=`$AXE check $MODEL $LOG/trace.axe 2>> $LOG/errors.txt`
    if [ "$OUTCOME" == "OK" ]; then
      echo -n .
    else
      if [ "$OUTCOME" == "NO" ]; then
        echo -e "\n\nFailed $MODEL with seed $I"
        echo "See $LOG/trace.txt and $LOG/trace.axe for counterexample"
        exit -1
      else
        echo -e "\n\nError during trace generation with seed $I"
        echo "See $LOG/errors.txt for details"
        exit -1
      fi
    fi
 fi
done

echo -e "\n\nOK, passed $NUM_TESTS tests"
$TRACE_STATS $LOG/stats.txt
