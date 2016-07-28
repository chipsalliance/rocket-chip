#!/usr/bin/env python

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

# -------
# Outline
# -------

# Usage:
#
#   tracegen-stats.py STATS-FILE
#
# This script produces some statistics about the traces generated
# using tracegen.py.

import sys
import subprocess
import re

def main():
  if len(sys.argv) != 2:
    sys.stderr.write("Usage: tracegen-stats.py STATS-FILE\n")
    sys.exit(-1)

  f = open(sys.argv[1], 'r')
  if f == None:
    sys.stderr.write("File not found: " + sys.argv[1] + "\n")
    sys.exit(-1)

  lrscSuccessSum = 0.0
  lrscSuccessCount = 0
  loadExtRateSum = 0.0
  loadExtRateCount = 0
  for line in f:
    if line[0:18] == "LRSC_Success_Rate=":
      val = float(line[18:-1])
      lrscSuccessSum = lrscSuccessSum + val
      lrscSuccessCount = lrscSuccessCount + 1

    if line[0:19] == "Load_External_Rate=":
      val = float(line[19:-1])
      loadExtRateSum = loadExtRateSum + val
      loadExtRateCount = loadExtRateCount + 1

  if lrscSuccessCount > 0:
    lrscSuccessAvg = lrscSuccessSum / float(lrscSuccessCount)
    lrscSuccessRate = str(int(100.0*lrscSuccessAvg)) + "%"
    print "LR/SC success rate:", lrscSuccessRate
  else:
    print "LR/SC success rate: none performed"

  if loadExtRateCount > 0:
    loadExtRateAvg = loadExtRateSum / float(loadExtRateCount)
    loadExtRate = str(int(100.0*loadExtRateAvg)) + "%"
    print "Load-external rate:", loadExtRate
  else:
    print "Load-external rate: none performed"

try:
  main()
except KeyboardInterrupt:
  sys.exit(-1)
