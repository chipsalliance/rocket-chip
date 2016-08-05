# groundtest

A memory tester circuit for Rocket Chip's memory system. The generator tile
plugs into the existing SoC generator as what looks like a CPU. However,
instead of running programs, the tile generates fixed memory requests out to
the L2. There are both cached and uncached generators. The cached generator
has an intervening L1 cache, the uncached generator sends TileLink requests
directly to the L2.

Assertions are set to fail if the wrong data comes back or if a request times
out waiting for the response.

## Configuring Rocket-Chip with groundtest

The groundtest package defines a GroundTestTile, which extends a
rocket-chip Tile.  A number of Configs in rocket-chip instantiate
GroundTestTile(s) in place of other types of Tiles (see
[TestConfigs.scala](https://github.com/ucb-bar/rocket-chip/blob/master/src/main/scala/TestConfigs.scala)).

Running a ground test can be achieved in rocket-chip as follows
(assuming the `build.sh` script in the
`rocket-chip/riscv-tools` directory has already been run).

```
cd emulator
make CONFIG=<GroundTestConfigName>
ln -s ../riscv-tools/riscv-tests/build/isa/rv64ui-p-simple
./emulator-Top-<GroundTestConfigName> rv64ui-p-simple <other args>
```

Currently the Configs which include GroundTestTile(s) are:

- MemtestConfig
- MemtestL2Config
- BroadcastRegressionTestConfig
- CacheRegressionTestConfig
- UnitTestConfig
- TraceGenConfig
- ComparatorConfig
- ComparatorL2Config

The usual Make targets run-asm-tests and run-bmark-tests still work for these configurations, though they don't do much.

## Using TraceGenConfig

The trace generator in groundtest
([tracegen.scala](https://github.com/ucb-bar/groundtest/blob/master/src/main/scala/tracegen.scala)) has the ability to generate random memory-subsystem traces, i.e. random sequences of memory requests, along with their responses. The idea is that these traces can be validated by an external checker, such as [axe](https://github.com/CTSRD-CHERI/axe).

Putting the generator and the checker together, we can automatically search for invalid traces, i.e. possible bugs in the memory subsystem. This is useful for intensive testing, but also debugging: it is possible to search for simple failing cases.

### Quick Reference

The [tracegen+check.sh](https://github.com/ucb-bar/groundtest/blob/master/scripts/tracegen%2Bcheck.sh) script provides an automated way to run a number of randomized tests. The number of tests, initial seed, and other parameters can be set via environment variables or the command line, see the script for more details. 

Before running the script, first ensure that:

- the file `rocket-chip/riscv-tools/riscv-tests/build/isa/rv64ui-p-simple`
  exists (this is produced by the `build.sh` script in the
  `rocket-chip/riscv-tools` directory);
- `rocket-chip/groundtest/scripts` in your `PATH`;
- `rocket-chip/emulator` is your current working directory.

Now the script can be run as follows.

```
> make CONFIG=TraceGenConfig
> tracegen+check.sh
Testing against WMO model:
 
       0: .......... .......... .......... .......... .......... 
      50: .......... .......... .......... .......... ..........

OK, passed 100 tests
LR/SC success rate: 88%
Load-external rate: 45%
```

### Running Manually

Suppose we have built the Rocket Chip emulator with the TraceGenConfig
configuration as above. Running it using the
[tracegen.py](https://github.com/ucb-bar/groundtest/blob/master/scripts/tracegen.py)
wrapper script with a few command-line options gives us a random
trace:

```
  > tracegen.py ./emulator-Top-TraceGenConfig 1 rv64ui-p-simple
  1: load-req     0x0000000008 #0 @64
  1: store-req  5 0x0000100008 #1 @65
  1: store-req  7 0x0000000010 #2 @66
  0: store-req  2 0x0000000008 #0 @303
  0: load-req     0x0000000008 #1 @304
  0: store-req  6 0x0000100008 #2 @305
  1: resp       0              #0 @96
  0: resp       0              #0 @350
  0: resp       2              #1 @351
  0: load-req     0x0000000010 #3 @353
  1: resp       0              #1 @149
  1: load-req     0x0000000108 #3 @152
  1: resp       0              #3 @184
  0: resp       5              #2 @422
  0: resp       0              #3 @424
  1: resp       0              #2 @226
  ...
```

Main points:

- the second command-line option sets the random seed;
- the first number on each line of the trace is the core id;
- \#N denotes a request-id N;
- @T denotes a time T in clock cycles;
- hex numbers denote addresses;
- remaining decimal numbers denote values being loaded or stored;
- the value written by every store is unique (this simplifies trace checking and reasoning);
- this trace contains only loads, stores and responses, but the generator (and axe) also support LR/SC pairs, atomics, and fences.


We convert these traces to axe format using the
[toaxe.py](https://github.com/ucb-bar/groundtest/blob/master/scripts/toaxe.py) script.

```
  > tracegen.py ./emulator-Top-TraceGenConfig 1 rv64ui-p-simple | toaxe.py -
  # &M[2] == 0x0000000010
  # &M[0] == 0x0000000008
  # &M[3] == 0x0000000108
  # &M[1] == 0x0000100008
  1: M[0] == 0 @ 64:96
  1: M[1] := 5 @ 65:
  1: M[2] := 7 @ 66:
  0: M[0] := 2 @ 303:
  0: M[0] == 2 @ 304:351
  0: M[1] := 6 @ 305:
  0: M[2] == 0 @ 353:424
  1: M[3] == 0 @ 152:184
  ...
```

Main points:

- lines begining # are comments, showing the addresses being used;
- after @ are the optional begin and end times of the operation.

Axe traces can be validated using the [axe](https://github.com/CTSRD-CHERI/axe) tool (must be downloaded and installed seperately):
```
> tracegen.py ./emulator-Top-TraceGenConfig 1 rv64ui-p-simple | toaxe.py - | axe check WMO -
OK
```

Axe reports that this trace is valid according to the WMO model.
