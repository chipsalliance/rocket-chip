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
#   tracegen.py EMULATOR SEED
#
# This script generates a trace using the given emulator (built
# with CONFIG=TraceGenConfig).  It waits until all cores have
# completed trace generation before terminating the emulator.

import sys
import subprocess
import re

def main():
  if len(sys.argv) != 3:
    sys.stderr.write("Usage: tracegen.py EMULATOR SEED\n")
    sys.exit(-1)

  p = subprocess.Popen([sys.argv[1],
         "+verbose", "-s" + sys.argv[2]],
         stderr=subprocess.PIPE, stdout=subprocess.PIPE)
  if p == None:
    sys.stderr.write("File not found: " + sys.argv[1] + "\n")
    sys.exit(-1)

  numFinished = 0
  while True:
    line = p.stderr.readline()
    if line[0:9] == "FINISHED ":
      total = int(line[9:-1])
      numFinished = numFinished + 1
      if numFinished == total:
        break
    elif line[0:15] == "Completed after":
      break
    elif line[0:7] == "testing":
      continue
    else:
      print line,

  p.terminate()

try:
  main()
except KeyboardInterrupt:
  sys.exit(-1)
