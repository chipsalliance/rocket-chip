Rocket Chip Generator :rocket: [![Build Status](https://travis-ci.org/freechipsproject/rocket-chip.svg?branch=master)](https://travis-ci.org/freechipsproject/rocket-chip)
=====================

This repository contains the Rocket chip generator necessary to instantiate
the RISC-V Rocket Core. For more information on Rocket Chip, please consult our [technical report](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html).

## Table of Contents

+ [Quick instructions](#quick) for those who want to dive directly into the details without knowing exactly what's in the repository.
+ [What's in the Rocket chip generator repository?](#what)
+ [How should I use the Rocket chip generator?](#how)
    + [Using the cycle-accurate Verilator simulation](#emulator)
    + [Mapping a Rocket core down to an FPGA](#fpga)
    + [Pushing a Rocket core through the VLSI tools](#vlsi)
+ [How can I parameterize my Rocket chip?](#param)
+ [Contributors](#contributors)

## <a name="quick"></a> Quick Instructions

### Checkout The Code

    $ git clone https://github.com/ucb-bar/rocket-chip.git
    $ cd rocket-chip
    $ git submodule update --init

### Setting up the RISCV environment variable

To build the rocket-chip repository, you must point the RISCV
environment variable to your riscv-tools installation directory. 

    $ export RISCV=/path/to/riscv/toolchain/installation
    
The riscv-tools repository is already included in 
rocket-chip as a git submodule. You **must** build this version 
of riscv-tools:

    $ cd rocket-chip/riscv-tools
    $ git submodule update --init --recursive
    $ export RISCV=/path/to/install/riscv/toolchain
    $ export MAKEFLAGS="$MAKEFLAGS -jN" # Assuming you have N cores on your host system
    $ ./build.sh
    $ ./build-rv32ima.sh (if you are using RV32).

For more information (or if you run into any issues), please consult the
[riscv-tools/README](https://github.com/riscv/riscv-tools/blob/master/README.md).

### Install Necessary Dependencies

You may need to install some additional packages to use this repository.
Rather than list all dependencies here, please see the appropriate section of the READMEs for each of the subprojects:

* [riscv-tools "Ubuntu Packages Needed"](https://github.com/riscv/riscv-tools/blob/priv-1.10/README.md#quickstart)
* [chisel3 "Installation"](https://github.com/ucb-bar/chisel3#installation)

### Building The Project

First, to build the C simulator:

    $ cd emulator
    $ make

Or to build the VCS simulator:

    $ cd vsim
    $ make

In either case, you can run a set of assembly tests or simple benchmarks
(Assuming you have N cores on your host system):

    $ make -jN run-asm-tests
    $ make -jN run-bmark-tests

To build a C simulator that is capable of VCD waveform generation:

    $ cd emulator
    $ make debug

And to run the assembly tests on the C simulator and generate waveforms:

    $ make -jN run-asm-tests-debug
    $ make -jN run-bmark-tests-debug

To generate FPGA- or VLSI-synthesizable verilog (output will be in `vsim/generated-src`):

    $ cd vsim
    $ make verilog


### Keeping Your Repo Up-to-Date

If you are trying to keep your repo up to date with this github repo,
you also need to keep the submodules and tools up to date.

    $ # Get the newest versions of the files in this repo
    $ git pull origin master
    $ # Make sure the submodules have the correct versions
    $ git submodule update --init --recursive

If riscv-tools version changes, you should recompile and install riscv-tools according to the directions in the [riscv-tools/README](https://github.com/riscv/riscv-tools/blob/master/README.md).

    $ cd riscv-tools
    $ ./build.sh
    $ ./build-rv32ima.sh (if you are using RV32)

## <a name="what"></a> What's in the Rocket chip generator repository?

The rocket-chip repository is a meta-repository that points to several
sub-repositories using [Git submodules](http://git-scm.com/book/en/Git-Tools-Submodules). 
Those repositories contain tools needed to generate and test SoC designs.
This respository also contains code that is used to generate RTL.
Hardware generation is done using [Chisel](http://chisel.eecs.berkeley.edu),
a hardware construction language embedded in Scala.
The rocket-chip generator is a Scala program that invokes the Chisel compiler
in order to emit RTL describing a complete SoC.
The following sections describe the components of this repository.

### <a name="what_submodules"></a>Git Submodules

[Git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules) allow you to keep a Git repository as a subdirectory of another Git repository.
For projects being co-developed with the Rocket Chip Generator, we have often found it expedient to track them as submodules,
allowing for rapid exploitation of new features while keeping commit histories separate.
As submoduled projects adopt stable public APIs, we transition them to external dependencies.
Here are the submodules that are currently being tracked in the rocket-chip repository:

* **chisel3**
([https://github.com/ucb-bar/chisel3](https://github.com/ucb-bar/chisel3)):
The Rocket Chip Generator uses [Chisel](http://chisel.eecs.berkeley.edu) to generate RTL.
* **firrtl**
([https://github.com/ucb-bar/firrtl](https://github.com/ucb-bar/firrtl)):
[Firrtl (Flexible Internal Representation for RTL)](http://bar.eecs.berkeley.edu/projects/2015-firrtl.html)
is the intermediate representation of RTL constructions used by Chisel3.
The Chisel3 compiler generates a Firrtl representation,
from which the final product (Verilog code, C code, etc) is generated.
* **hardfloat**
([https://github.com/ucb-bar/berkeley-hardfloat](https://github.com/ucb-bar/berkeley-hardfloat)):
Hardfloat holds Chisel code that generates parameterized IEEE 754-2008 compliant
floating-point units used for fused multiply-add operations, conversions
between integer and floating-point numbers, and conversions between
floating-point conversions with different precision.
* **riscv-tools**
([https://github.com/riscv/riscv-tools](https://github.com/riscv/riscv-tools)):
We tag a version of the RISC-V software ecosystem that works with the RTL committed in this repository.
* **torture**
([https://github.com/ucb-bar/riscv-torture](https://github.com/ucb-bar/riscv-torture)):
This module is used to generate and execture constrained random instruction streams that can
be used to stress-test both the core and uncore portions of the design.

### <a name="what_packages"></a>Scala Packages

In addition to submodules that track independent git repositories,
the rocket-chip code base is itself factored into a number of Scala packages.
These packages are all found within the src/main/scala directory.
Some of these packages provide Scala utilities for generator configuration,
while other contain the actual Chisel RTL generators themselves.
Here is a brief description of what can be found in each package:

* **amba**
This RTL package uses diplomacy to generate bus implementations of AMBA protocols, including AXI4, AHB-lite, and APB.
* **config**
This utility package provides Scala interfaces for configuring a generator via a dynamically-scoped
parameterization library.
* **coreplex**
This RTL package generates a complete coreplex by gluing together a variety of components from other packages,
including: tiled Rocket cores, a system bus network, coherence agents, debug devices, interrupt handlers, externally-facing peripherals,
clock-crossers and converters from TileLink to external bus protocols (e.g. AXI or AHB).
* **devices**
This RTL package contains implementations for peripheral devices, including the Debug module and various TL slaves.
* **diplomacy**
This utility package extends Chisel by allowing for two-phase hardware elaboration, in which certain parameters
are dynamically negotiated between modules.
* **groundtest**
This RTL package generates synthesizeable hardware testers that emit randomized
memory access streams in order to stress-tests the uncore memory hierarchy.
* **jtag**
This RTL package provides definitions for generating JTAG bus interfaces. 
* **regmapper**
This utility package generates slave devices with a standardized interface for accessing their memory-mapped registers.
* **rocket**
This RTL package generates the Rocket in-order pipelined core,
as well as the L1 instruction and data caches.
This library is intended to be used by a chip generator that instantiates the
core within a memory system and connects it to the outside world.
* **tile**
This RTL package contains components that can be combined with cores to construct tiles, such as FPUs and accelerators.
* **tilelink**
This RTL package uses diplomacy to generate bus implementations of the TileLink protocol. It also contains a variety
of adapters and protocol converters.
* **system**
This top-level utility package invokes Chisel to elaborate a particular configuration of a coreplex,
along with the appropriate testing collateral.
* **unittest**
This utility package contains a framework for generateing synthesizeable hardware testers of individual modules.
* **util**
This utility package provides a variety of common Scala and Chisel constructs that are re-used across
multiple other packages,

### <a name="what_else"></a>Other Resources

Outside of Scala, we also provide a variety of resources to create a complete SoC implementation and
test the generated designs.

* **bootrom**
Sources for the first-stage bootloader included in the BootROM.
* **csrc**
C sources for use with Verilator simulation.
* **emulator**
Directory in which Verilator simulations are compiled and run.
* **project**
Directory used by SBT for Scala compilation and build.
* **regression**
Defines continuous integration and nightly regression suites.
* **scripts**
Utilities for parsing the output of simulations or manipulating the contents of source files.
* **vsim**
Directory in which Synopsys VCS simulations are compiled and run.
* **vsrc**
Verilog sources containing interfaces, harnesses and VPI.


### <a name="what_toplevel"></a>Extending the Top-Level Design

See [this description](https://github.com/ucb-bar/project-template) of how to create
you own top-level design with custom devices.

## <a name="how"></a> How should I use the Rocket chip generator?

Chisel can generate code for three targets: a high-performance
cycle-accurate Verilator, Verilog optimized for FPGAs, and Verilog
for VLSI. The rocket-chip generator can target all three backends.  You
will need a Java runtime installed on your machine, since Chisel is
overlaid on top of [Scala](http://www.scala-lang.org/). Chisel RTL (i.e.
rocket-chip source code) is a Scala program executing on top of your
Java runtime. To begin, ensure that the ROCKETCHIP environment variable
points to the rocket-chip repository.

    $ git clone https://github.com/ucb-bar/rocket-chip.git
    $ cd rocket-chip
    $ export ROCKETCHIP=`pwd`
    $ git submodule update --init
    $ cd riscv-tools
    $ git submodule update --init --recursive riscv-tests

Before going any further, you must point the RISCV environment variable
to your riscv-tools installation directory. If you do not yet have
riscv-tools installed, follow the directions in the
[riscv-tools/README](https://github.com/riscv/riscv-tools/blob/master/README.md).

    export RISCV=/path/to/install/riscv/toolchain

Otherwise, you will see the following error message while executing any
command in the rocket-chip generator:

    *** Please set environment variable RISCV. Please take a look at README.

### <a name="emulator"></a> 1) Using the high-performance cycle-accurate Verilator

Your next step is to get the Verilator working. Assuming you have N
cores on your host system, do the following:

    $ cd $ROCKETCHIP/emulator
    $ make -jN run

By doing so, the build system will generate C++ code for the
cycle-accurate emulator, compile the emulator, compile all RISC-V
assembly tests and benchmarks, and run both tests and benchmarks on the
emulator. If make finished without any errors, it means that the
generated Rocket chip has passed all assembly tests and benchmarks!

You can also run assembly tests and benchmarks separately:

    $ make -jN run-asm-tests
    $ make -jN run-bmark-tests

To generate vcd waveforms, you can run one of the following commands:

    $ make -jN run-debug
    $ make -jN run-asm-tests-debug
    $ make -jN run-bmark-tests-debug

Or call out individual assembly tests or benchmarks:

    $ make output/rv64ui-p-add.out
    $ make output/rv64ui-p-add.vcd

Now take a look in the emulator/generated-src directory. You will find
Chisel generated verilog code and its associated C++ code generated by
verilator.

    $ ls $ROCKETCHIP/emulator/generated-src
    DefaultConfig.dts
    DefaultConfig.graphml
    DefaultConfig.json
    DefaultConfig.memmap.json
    freechips.rocketchip.system.DefaultConfig
    freechips.rocketchip.system.DefaultConfig.d
    freechips.rocketchip.system.DefaultConfig.fir
    freechips.rocketchip.system.DefaultConfig.v
    $ ls $ROCKETCHIP/emulator/generated-src/freechips.rocketchip.system.DefaultConfig
    VTestHarness__1.cpp
    VTestHarness__2.cpp
    VTestHarness__3.cpp
    ...

Also, output of the executed assembly tests and benchmarks can be found
at emulator/output/\*.out. Each file has a cycle-by-cycle dump of
write-back stage of the pipeline. Here's an excerpt of
emulator/output/rv64ui-p-add.out:

    C0: 483 [1] pc=[00000002138] W[r 3=000000007fff7fff][1] R[r 1=000000007fffffff] R[r 2=ffffffffffff8000] inst=[002081b3] add s1, ra, s0
    C0: 484 [1] pc=[0000000213c] W[r29=000000007fff8000][1] R[r31=ffffffff80007ffe] R[r31=0000000000000005] inst=[7fff8eb7] lui t3, 0x7fff8
    C0: 485 [0] pc=[00000002140] W[r 0=0000000000000000][0] R[r 0=0000000000000000] R[r 0=0000000000000000] inst=[00000000] unknown

The first [1] at cycle 483, core 0, shows that there's a
valid instruction at PC 0x2138 in the writeback stage, which is
0x002081b3 (add s1, ra, s0). The second [1] tells us that the register
file is writing r3 with the corresponding value 0x7fff7fff. When the add
instruction was in the decode stage, the pipeline had read r1 and r2
with the corresponding values next to it. Similarly at cycle 484,
there's a valid instruction (lui instruction) at PC 0x213c in the
writeback stage. At cycle 485, there isn't a valid instruction in the
writeback stage, perhaps, because of a instruction cache miss at PC
0x2140.

### <a name="fpga"></a> 2) Mapping a Rocket core to an FPGA

You can generate synthesizable Verilog with the following commands:

    $ cd $ROCKETCHIP/vsim
    $ make verilog CONFIG=DefaultFPGAConfig

The Verilog used for the FPGA tools will be generated in
vsim/generated-src. Please proceed further with the directions shown in
the [README](https://github.com/ucb-bar/fpga-zynq/blob/master/README.md)
of the fpga-zynq repository.


If you have access to VCS, you will be able to run assembly
tests and benchmarks in simulation with the following commands
(again assuming you have N cores on your host machine):

    $ cd $ROCKETCHIP/vsim
    $ make -jN run CONFIG=DefaultFPGAConfig

The generated output looks similar to those generated from the emulator.
Look into vsim/output/\*.out for the output of the executed assembly
tests and benchmarks.

### <a name="vlsi"></a> 3) Pushing a Rocket core through the VLSI tools

You can generate Verilog for your VLSI flow with the following commands:

    $ cd $ROCKETCHIP/vsim
    $ make verilog

Now take a look at vsim/generated-src, and the contents of the
Top.DefaultConfig.conf file:

    $ cd $ROCKETCHIP/vsim/generated-src
    DefaultConfig.dts
    DefaultConfig.graphml
    DefaultConfig.json
    DefaultConfig.memmap.json
    freechips.rocketchip.system.DefaultConfig.behav_srams.v
    freechips.rocketchip.system.DefaultConfig.conf
    freechips.rocketchip.system.DefaultConfig.d
    freechips.rocketchip.system.DefaultConfig.fir
    freechips.rocketchip.system.DefaultConfig.v
    $ cat $ROCKETCHIP/vsim/generated-src/*.conf
    name data_arrays_0_ext depth 512 width 256 ports mrw mask_gran 8
    name tag_array_ext depth 64 width 88 ports mrw mask_gran 22
    name tag_array_0_ext depth 64 width 84 ports mrw mask_gran 21
    name data_arrays_0_1_ext depth 512 width 128 ports mrw mask_gran 32
    name mem_ext depth 33554432 width 64 ports mwrite,read mask_gran 8
    name mem_2_ext depth 512 width 64 ports mwrite,read mask_gran 8

The conf file contains information for all SRAMs instantiated in the
flow. If you take a close look at the $ROCKETCHIP/Makefrag, you will see
that during Verilog generation, the build system calls a $(mem\_gen)
script with the generated configuration file as an argument, which will
fill in the Verilog for the SRAMs. Currently, the $(mem\_gen) script
points to vsim/vlsi\_mem\_gen, which simply instantiates behavioral
SRAMs.  You will see those SRAMs being appended at the end of
vsim/generated-src/Top.DefaultConfig.v. To target vendor-specific
SRAMs, you will need to make necessary changes to vsim/vlsi\_mem\_gen.

Similarly, if you have access to VCS, you can run assembly tests and
benchmarks with the following commands (again assuming you have N cores
on your host machine):

    $ cd $ROCKETCHIP/vsim
    $ make -jN run
 
The generated output looks similar to those generated from the emulator.
Look into vsim/output/\*.out for the output of the executed assembly
tests and benchmarks.

## <a name="param"></a> How can I parameterize my Rocket chip?

By now, you probably figured out that all generated files have a configuration
name attached, e.g. DefaultConfig. Take a look at
src/main/scala/rocketchip/Configs.scala. Search for NSets and NWays defined in
BaseConfig. You can change those numbers to get a Rocket core with different
cache parameters. For example, by changing L1I, NWays to 4, you will get
a 32KB 4-way set-associative L1 instruction cache rather than a 16KB 2-way
set-associative L1 instruction cache.

Further down, you will be able to see two FPGA configurations:
DefaultFPGAConfig and DefaultFPGASmallConfig. DefaultFPGAConfig inherits from
BaseConfig, but overrides the low-performance memory port (i.e., backup
memory port) to be turned off. This is because the high-performance memory
port is directly connected to the high-performance AXI interface on the ZYNQ
FPGA. DefaultFPGASmallConfig inherits from DefaultFPGAConfig, but changes the
cache sizes, disables the FPU, turns off the fast early-out multiplier and
divider, and reduces the number of TLB entries (all defined in SmallConfig).
This small configuration is used for the Zybo FPGA board, which has the
smallest ZYNQ part.

Towards the end, you can also find that DefaultSmallConfig inherits all
parameters from BaseConfig but overrides the same parameters of
WithNSmallCores.

Now take a look at vsim/Makefile. Search for the CONFIG variable.
By default, it is set to DefaultConfig.  You can also change the
CONFIG variable on the make command line:

    $ cd $ROCKETCHIP/vsim
    $ make -jN CONFIG=DefaultSmallConfig run-asm-tests

Or, even by defining CONFIG as an environment variable:

    $ export CONFIG=DefaultSmallConfig
    $ make -jN run-asm-tests

This parameterization is one of the many strengths of processor
generators written in Chisel, and will be more detailed in a future blog
post, so please stay tuned.

To override specific configuration items, such as the number of external interrupts,
you can create your own Configuration(s) and compose them with Config's ++ operator

    class WithNExtInterrupts(nExt: Int) extends Config {
        (site, here, up) => {
            case NExtInterrupts => nExt
        }
    }
    class MyConfig extends Config (new WithNExtInterrupts(16) ++ new DefaultSmallConfig)

Then you can build as usual with CONFIG=MyConfig.

## <a name="contributors"></a> Contributors

Can be found [here](https://github.com/ucb-bar/rocket-chip/graphs/contributors).

## <a name="attribution"></a> Attribution

If used for research, please cite Rocket Chip by the technical report:

Krste Asanović, Rimas Avižienis, Jonathan Bachrach, Scott Beamer, David Biancolin, Christopher Celio, Henry Cook, Palmer Dabbelt, John Hauser, Adam Izraelevitz, Sagar Karandikar, Benjamin Keller, Donggyu Kim, John Koenig, Yunsup Lee, Eric Love, Martin Maas, Albert Magyar, Howard Mao, Miquel Moreto, Albert Ou, David Patterson, Brian Richards, Colin Schmidt, Stephen Twigg, Huy Vo, and Andrew Waterman, _[The Rocket Chip Generator](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html)_, Technical Report UCB/EECS-2016-17, EECS Department, University of California, Berkeley, April 2016
