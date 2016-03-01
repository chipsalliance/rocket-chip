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

# This script takes memory-subsystem traces produced by the groundtest
# trace generator (see tracegen.scala) and puts them into a format
# that can be validated by the axe tool (see
# https://github.com/CTSRD-CHERI/axe).

import sys
import re

if len(sys.argv) != 2:
  print "Usage: toaxe.py [FILE]"
  sys.exit()

if sys.argv[1] == "-":
  f = sys.stdin
else:
  f = open(sys.argv[1], 'r')
  if f == None:
    print "File not found: ", sys.argv[1]
    sys.exit()

lineCount = 0
def error(msg):
  print "Error at line ", lineCount, ": ", msg
  sys.exit()

# Mapping from address to axe address
addrMap = {}
nextAddr = 0

# Mapping from (thread id, tag id) to axe operation id
tagMap = {}

# Mapping from thread id to operation id
fenceReq = {}

# Mapping from thread id to operation id
loadReserve = {}

# Array of axe operations
ops = []

for line in f:
  # Exit loop at end of trace
  if line[0:9] == 'Completed': break

  # Parse thread id and command
  m = re.search(' *([0-9]+) *: *([^ ]*) (.*)', line)
  if m == None: error("Expected: <thread-id> ':' <command>")
  tid, cmd, line = m.group(1), m.group(2), m.group(3)

  if cmd == 'fence-req':
    # Parse time
    m = re.search(' *@ *([0-9]+)', line)
    if m == None: error ("expected timestamp")
    # Insert placeholder containing request time
    ops.append(str(m.group(1)))
    fenceReq[tid] = len(ops)-1
  elif cmd == 'fence-resp':
    # Insert 'sync' operation
    if not (tid in fenceReq) or fenceReq[tid] == None:
      error("fence-resp without fence-req on thread " + tid)
    startTime = ops[fenceReq[tid]]
    op = str(tid) + ": sync @ " + startTime
    # Add end-time
    m = re.search(' *@ *([0-9]+)', line)
    if m != None: op = op + ":" + str(m.group(1))
    ops[fenceReq[tid]] = (op,)
    fenceReq[tid] = None
  elif cmd == 'load-req' or cmd == 'load-reserve-req':
    # Parse address, tag, and time
    m = re.search(' *([0-9a-fx]+) *# *([0-9]+) *@ *([0-9]+)', line)
    if m == None: error("expected <address> #<tag> @<timestamp>")
    # Update address map
    if not (m.group(1) in addrMap):
      addrMap[m.group(1)] = nextAddr
      nextAddr = nextAddr+1
    # Insert place-holder
    ops.append((cmd, None, addrMap[m.group(1)], m.group(3), None))
    tagMap[(tid, m.group(2))] = len(ops)-1
    if cmd == 'load-reserve-req':
      loadReserve[tid] = len(ops)-1
  elif cmd == 'store-req' or cmd == 'store-cond-req' or cmd == 'swap-req':
    # Parse value, address, tag, and time
    m = re.search(' *([0-9]+) *([0-9a-fx]+) *# *([0-9]+) *@ *([0-9]+)', line)
    if m == None: error("expected <value> <address> #<tag> @<timestamp>")
    # Update address map
    if not (m.group(2) in addrMap):
      addrMap[m.group(2)] = nextAddr
      nextAddr = nextAddr+1
    # Insert place-holder
    lr = loadReserve[tid] if tid in loadReserve else None
    ops.append((cmd, m.group(1), addrMap[m.group(2)], m.group(4), lr))
    tagMap[(tid, m.group(3))] = len(ops)-1
    if cmd == 'store-cond-req': loadReserve[tid] = None
  elif cmd == 'resp':
    # Parse value and timestamp
    m = re.search(' *([0-9]+) *# *([0-9]+) *@ *([0-9]+)', line)
    if m == None: error("expected <value> #<tag> @<timestamp>")
    # Find corresponding response
    tag = m.group(2)
    if not ((tid, tag) in tagMap) or tagMap[(tid, tag)] == None:
      error("resp without associated req with tag " + tag + " on thread " + tid)
    opId = tagMap[(tid, tag)]
    (c, val, addr, start, lr) = ops[opId]
    if c == 'load-req':
      op = tid + ": M[" + str(addr) + '] == ' + m.group(1) + ' @ '
      op += start + ':' + m.group(3)
      ops[opId] = (op,)
    elif c == 'store-req':
      op = tid + ": M[" + str(addr) + '] := ' + val + ' @ '
      op += start + ':' # + m.group(3)
      ops[opId] = (op,)
    elif c == 'load-reserve-req':
      ops[opId] = (m.group(1), start, m.group(3))
    elif c == 'store-cond-req':
      if lr == None: error("store conditional without load-reserve")
      (loadVal, loadStart, loadFin) = ops[lr]
      if int(m.group(1)) != 0:
        # SC fail
        op = tid + ": M[" + str(addr) + "] == " + loadVal
        op += " @ " + loadStart + ":" + loadFin
      else:
        # SC success
        op = tid + ": { M[" + str(addr) + "] == " + loadVal + "; "
        op += "M[" + str(addr) + "] := " + val + "} @ "
        op += loadStart + ":" # + m.group(3)
      ops[lr] = (op,)
      ops[opId] = None
    elif c == 'swap-req':
      op = tid + ": { M[" + str(addr) + '] == ' + m.group(1)
      op += '; M[' + str(addr) + '] := ' + val
      op += '} @ ' + start + ':' # + m.group(3)
      ops[opId] = (op,)
  else:
    error("Unknown command '" + cmd + "'")
    
  lineCount = lineCount+1

# Print address map in comments
for addr in addrMap:
  print ("# &M[" + str(addrMap[addr]) + "] == " + addr)

# Print axe trace
for op in ops:
  if op != None and isinstance(op, tuple) and len(op) == 1:
    print op[0]
