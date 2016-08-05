import sys
from collections import defaultdict

# Checks a trace of cache transactions to make sure the data is correct
# Note: this will only work if the L2 agent only receives cached traffic
# (either caching Acquires or Releases). If there are any builtin
# Put or PutBlock requests, they will not be reflected in the trace
# and the data will appear to be incorrect.

DATA_BEATS = 8

def parse_prm(fname):
    mem_data_bits = 0
    cache_block_bytes = 0
    with open(fname) as f:
        for line in f:
            line = line.strip("() \t\n")
            parts = line.split(",")
            if parts[0] == "MEM_DATA_BITS":
                mem_data_bits = int(parts[1])
            elif parts[1] == "CACHE_BLOCK_BYTES":
                cache_block_bytes = int(parts[1])
    DATA_BEATS = (cache_block_bytes * 8) / mem_data_bits

def data_block():
    return [0] * DATA_BEATS

blocks = defaultdict(data_block)

def process_release(addr_block, addr_beat, data):
    blocks[addr_block][addr_beat] = data

def process_get(addr_block, addr_beat, data):
    stored_data = blocks[addr_block][addr_beat]
    if stored_data != data:
        print("Error {:07x},{}: {:016x} != {:016x}".format(
            addr_block, addr_beat, stored_data, data))

def process_line(line):
    if not line:
        return
    pieces = line.split()
    if pieces[0] == "[release]":
        addr_block = int(pieces[1].split('=')[1], 16)
        addr_beat  = int(pieces[2].split('=')[1])
        data       = int(pieces[3].split('=')[1], 16)
        process_release(addr_block, addr_beat, data)
    if pieces[0] == "[get]":
        addr_block = int(pieces[1].split('=')[1], 16)
        addr_beat  = int(pieces[2].split('=')[1])
        data       = int(pieces[3].split('=')[1], 16)
        process_get(addr_block, addr_beat, data)

def check_trace(fname):
    with open(fname) as f:
        for line in f:
            process_line(line.strip())

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: {} trace.out [params.prm]".format(sys.argv[0]))
        sys.exit(-1)
    if len(sys.argv) > 2:
        parse_prm(sys.argv[2])
    check_trace(sys.argv[1])
