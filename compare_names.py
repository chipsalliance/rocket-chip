#!/usr/bin/env python3

import argparse
import re
from pathlib import Path
from functools import reduce

parser = argparse.ArgumentParser(description="compares verilog names across chisel generations")

parser.add_argument(
    "--original-file", help="original generated verilog", required=True
)
parser.add_argument(
    "--modified-files", help="verilog files from different generators to compare to the original", nargs='+', required=True
)
parser.add_argument(
    "--short", help="only list names for shared modules and counts for different modules", action='store_true', default=False, required=False
)
args = parser.parse_args()

def get_module_names(contents):
    pattern = "^module\s+([A-Za-z_][A-Za-z_0-9]*_[A-F0-9]{8}).*"
    return set(re.findall(pattern, contents, re.MULTILINE))

def compareNames(short, pair1, pair2):
    file1 = pair1[0]
    names1 = pair1[1]
    file2 = pair2[0]
    names2 = pair2[1]
    same_names = [f"  {name}\n" for name in sorted(names1.intersection(names2))]
    diff1_names = [f"  {name}\n" for name in sorted(names1.difference(names2))]
    diff2_names = [f"  {name}\n" for name in sorted(names2.difference(names1))]
    print(f"===== comparing {file2} =====")
    if short:
        print(f"shared names ({len(same_names)}):\n" + "".join(same_names), end="")
        print(f"names only in {file1}: {len(diff1_names)}")
        print(f"names only in {file2}: {len(diff2_names)}")
    else:
        print(f"shared names ({len(same_names)}):\n" + "".join(same_names), end="")
        print(f"names only in {file1}:\n" + "".join(diff1_names))
        print(f"names only in {file2}:\n" + "".join(diff2_names))
    print()
    return pair2

def main():
    contents = [Path(f).read_text() for f in args.modified_files]
    names = [get_module_names(c) for c in contents]
    pairs = list(zip(args.modified_files, names))
    original = [args.original_file, get_module_names(Path(args.original_file).read_text())]
    for pair in pairs:
        compareNames(args.short, pair, original)

if __name__ == "__main__":
    main()
