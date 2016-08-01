import sys
import re

N_BLOCKS = 4
N_BEATS = 8
N_BYTES = 8
N_OPS = 142

memory = [[list() for _ in range(0, N_BEATS)] for _ in range(0, N_BLOCKS)]
addresses = {}

def process_put_block(xid, addr_block, data):
    fullmask = (1 << (N_BYTES * 8)) - 1
    for i in range(0, N_BEATS):
        real_data = (data << i) + data
        memory[addr_block][i].append((xid, real_data & fullmask))

def expand_wmask(wmask):
    bitmask = 0
    bitmask_byte = 0xff

    for i in range(0, N_BYTES):
        if ((wmask >> i) & 1) == 1:
            bitmask |= bitmask_byte
        bitmask_byte <<= 8

    return bitmask

def process_put(xid, addr_block, addr_beat, data, wmask):
    fullmask = (1 << (N_BYTES * 8)) - 1
    bitmask = expand_wmask(wmask)
    old_data = memory[addr_block][addr_beat][-1][1]
    new_data = ((bitmask & data) | (~bitmask & old_data)) & fullmask
    memory[addr_block][addr_beat].append((xid, new_data))

ACQ_RE = re.compile(r"\[acq\s*(\d+)\]: (.*)")
PUT_BLOCK_RE = re.compile(
    r"PutBlock\(addr_block = ([0-9a-f]+), data = ([0-9a-f]+)\)")
PUT_BEAT_RE = re.compile(
    (r"Put\(addr_block = ([0-9a-f]+), addr_beat = ([0-9a-f]+), "
          r"data = ([0-9a-f]+), wmask = ([0-9a-f]+)\)"))
GET_BLOCK_RE = re.compile(
    r"GetBlock\(addr_block = ([0-9a-f]+)\)")
GET_BEAT_RE = re.compile(
    (r"Get\(addr_block = ([0-9a-f]+), addr_beat = (\d+), "
        "addr_byte = \d+, op_size = \d+\)"))
GNT_RE = re.compile(
    r"\[gnt\s*(\d+)\]: g_type = \d+, addr_beat = (\d+), data = ([0-9a-f]+)")

def check_data(gnt_id, addr_beat, data):
    addr_block = addresses[gnt_id]
    last_data = None
    for (put_id, put_data) in memory[addr_block][addr_beat]:
        if put_id > gnt_id:
            break
        last_data = put_data
    if last_data != data:
        print("Data mismatch at {} for {:07x}, {} - got:{:016x} exp:{:016x}".format(
            gnt_id, addr_block, addr_beat, data, last_data))

def process_input(f):
    for line in f:
        m = ACQ_RE.match(line)
        if m:
            acq_id = int(m.groups()[0])
            body = m.groups()[1]
            process_acquire(acq_id, body)
        m = GNT_RE.match(line)
        if m:
            gnt_id = int(m.groups()[0])
            addr_beat = int(m.groups()[1])
            data = int(m.groups()[2], 16)
            check_data(gnt_id, addr_beat, data)

def process_acquire(acq_id, body):
    m = PUT_BLOCK_RE.match(body)
    if m:
        addr_block = int(m.groups()[0], 16)
        data = int(m.groups()[1], 16)
        process_put_block(acq_id, addr_block, data)
        return
    m = PUT_BEAT_RE.match(body)
    if m:
        addr_block = int(m.groups()[0], 16)
        addr_beat = int(m.groups()[1], 16)
        data = int(m.groups()[2], 16)
        wmask = int(m.groups()[3], 16)
        process_put(acq_id, addr_block, addr_beat, data, wmask)
        return
    m = GET_BLOCK_RE.match(body)
    if m:
        addresses[acq_id] = int(m.groups()[0], 16)
        return
    m = GET_BEAT_RE.match(body)
    if m:
        addresses[acq_id] = int(m.groups()[0], 16)

def main():
    if len(sys.argv) < 2:
        process_input(sys.stdin)
    else:
        with open(sys.argv[1]) as f:
            process_input(f)

if __name__ == "__main__":
    main()
