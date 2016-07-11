Rocket Chip Generator [![Build Status](https://travis-ci.org/ucb-bar/rocket-chip.svg?branch=master)](https://travis-ci.org/ucb-bar/rocket-chip)
=====================

This repository contains the Rocket chip generator necessary to instantiate
the RISC-V Rocket Core. For more information on Rocket Chip, please consult our [technical report](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html).

## Table of Contents

+ [Quick instructions](#quick) for those who want to dive directly into the details without knowing exactly what's in the repository.
+ [What's in the Rocket chip generator repository?](#what)
+ [How should I use the Rocket chip generator?](#how)
    + [Using the high-performance cycle-accurate C++ emulator](#emulator)
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
    $ ./build.sh
   
For more information (or if you run into any issues), please consult the
[riscv-tools/README](https://github.com/riscv/riscv-tools/blob/master/README.md).


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

To generate FPGA-synthesizable verilog (output will be in `fsim/generated-src`):

    $ cd fsim
    $ make verilog

Similarly, to generate VLSI-synthesizable verilog (output will be in `vsim/generated-src`):

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

If firrtl version changes and you are using Chisel3, you may need to clean and recompile:

    $ cd firrtl
    $ sbt clean
    $ sbt assembly

## <a name="what"></a> What's in the Rocket chip generator repository?

The rocket-chip repository is the head git repository that points to
many sub-repositories (e.g. the riscv-tools repository) using [git
submodules](http://git-scm.com/book/en/Git-Tools-Submodules).  While
we're aware of the ongoing debate as to how meta-projects should be
managed (i.e. a big monolithic repository vs. smaller repositories
tracked as submodules), we've found that for our chip-building projects
at Berkeley, the ability to compose a subset of private and public
sub-repositories on a per-chip basis is a killer feature of git
submodule.

### <a name="what_submodules"></a>The Submodules

Here's a look at all the git submodules that are currently tracked in
the rocket-chip repository:

* **chisel2**
([https://github.com/ucb-bar/chisel](https://github.com/ucb-bar/chisel)):
At Berkeley, we write RTL in Chisel. For those who are not familiar
with Chisel, please go take a look at
[http://chisel.eecs.berkeley.edu](http://chisel.eecs.berkeley.edu). We
have submoduled a specific git commit tag of the Chisel compiler rather
than pointing to a versioned Chisel release as an external dependency;
so far we were developing Chisel and the rocket core at the same time,
and hence it was easiest to use submodule to track bleeding edge commits
to Chisel, which contained a bunch of new features and bug fixes. As
Chisel gets more stable, we will likely replace this submodule with an
external dependency.
* **chisel3**
([https://github.com/ucb-bar/chisel3](https://github.com/ucb-bar/chisel3)):
Chisel3 is a newer version of Chisel, which is based on FIRRTL. The Chisel
code in this repository is generally compatible with both Chisel2 and Chisel3.
The [chisel3/README](https://github.com/ucb-bar/chisel3/blob/master/README.md).gives 
instructions on how to build your design with Chisel3 instead of Chisel2.
* **firrtl**
([https://github.com/ucb-bar/firrtl](https://github.com/ucb-bar/firrtl)):
FIRRTL (Flexible Internal Representation for RTL) is the intermediate format
which Chisel3 is based upon. The Chisel3 compiler generates a FIRRTL representation,
from which the final product (Verilog code, C code, etc) is generated.
* **rocket**
([https://github.com/ucb-bar/rocket](https://github.com/ucb-bar/rocket)):
The rocket repository holds the actual source code of the Rocket core.
Note that the L1 blocking I$ and the L1 non-blocking D$ are considered
part of the core, and hence we keep the L1 cache source code in this
repository. This repository is not meant to stand alone; it needs to be
included in a chip repository (e.g.  rocket-chip) that instantiates the
core within a memory system and connects it to the outside world.
* **uncore**
([https://github.com/ucb-bar/uncore](https://github.com/ucb-bar/uncore)):
This repository implements the uncore logic, such as the coherence hub
(the agent that keeps multiple L1 D$ coherent). The definition of the
coherent interfaces between tiles ("tilelink") and the debug interface
also live in this repository.
* **junctions**
([https://github.com/ucb-bar/junctions](https://github.com/ucb-bar/junctions)):
This repository contains code and
converters for various bus protocols and interfaces. 
* **hardfloat**
([https://github.com/ucb-bar/berkeley-hardfloat](https://github.com/ucb-bar/berkeley-hardfloat)):
This repository holds the parameterized IEEE 754-2008 compliant
floating-point units for fused multiply-add operations, conversions
between integer and floating-point numbers, and conversions between
floating-point conversions with different precision. The floating-point
units in this repository work on an internal recoded format (exponent
has an additional bit) to handle subnormal numbers more efficiently in
the processor. Please take a look at the
[README](https://github.com/ucb-bar/berkeley-hardfloat/blob/master/README.md)
in the repository for more information.
* **context-dependent-environments**
([https://github.com/ucb-bar/context-dependent-environments](https://github.com/ucb-bar/context-dependent-environments)):
The rocket-chip Chisel code is highly parameterizable, and utilizes the classes in 
this subrepo to set and pass parameters to different levels of the design. Note that in 
Chisel2, this was handled by Chisel itself, but has been moved into a seperate
library for use with Chisel3. 
* **dramsim2**
([https://github.com/dramninjasUMD/DRAMSim2](https://github.com/dramninjasUMD/DRAMSim2)):
Currently, the DRAM memory system is implemented in the testbench. We
use dramsim2 to emulate DRAM timing.
* **fpga-zynq**
([https://github.com/ucb-bar/fpga-zynq](https://github.com/ucb-bar/fpga-zynq)):
We also tag a version of the FPGA infrastructure that works with the RTL
committed in the rocket-chip repository.
* **riscv-tools**
([https://github.com/riscv/riscv-tools](https://github.com/riscv/riscv-tools)):
We tag a version of riscv-tools that works with the RTL committed in the
rocket-chip repository.  Once the software toolchain stabilizes, we
might turn this submodule into an external dependency.
* **groundtest**
([https://github.com/ucb-bar/groundtest](https://github.com/ucb-bar/groundtest)):
This repository contains code which can test the uncore by generating randomized
instruction streams. It replaces the rocket processor with an instruction
stream generator to stress-test the uncore portions of the design.
* **torture**
([https://github.com/ucb-bar/torture](https://github.com/ucb-bar/torture)):
The torture test code is used to generate randomized instruction streams which
are then run as code on the rocket core(s). These are constrained random tests
to stress-test both the core and uncore portions of the design.

### <a name="what_toplevel"></a>The Top Level Module

Next, take a look at rocket-chip's src/main/scala directory.
This directory has the Chisel source files including the top level
RocketChip.scala, which
instantiates both a Rocket core and the uncore logic, and then glues
them together. Here's a brief overview of source files found in the
rocket-chip repository:

* **RocketChip.scala**: Top-level source file (Top is the top-level
module name), which instantiates a Rocket core, uncore logic, and glues
them together.
* **Network.scala**: This source file holds the crossbar network used in
the uncore for multi-core implementations.
* **Configs.scala**: This holds all the rocket-chip parameters.
Probably this file is the most important file for external users. We
will revisit this topic in the next section "How should I use the Rocket
chip generator?", and will also post a more detailed explanation of the
parameter infrastructure in the near future.
* **Backends.scala**: An example of how the Chisel compiler's VLSI
backend can be extended to route a pin named "init" to all SRAM blocks
used in the design.  This separation cleans up the source RTL of the
design, since we don't need to add all the vendor-specific stuff in the
Chisel source code, yet still can correctly hook up our particular
SRAMs. The transformation is just a "compiler pass" in the Chisel
backend that happens as the compiler translates the Chisel source code
down to Verilog. Pretty neat huh?
* **Vlsi.scala**: This file is pretty specific to our tapeouts. It
implements logic to interface with an arbitrary number of slow
single-ended digital I/Os when implementing a test chip.

Take a look at the top-level I/O pins. Open up
src/main/scala/RocketChip.scala, and search for TopIO. You will read the
following:

    /** Top-level io for the chip */
    class BasicTopIO(implicit val p: Parameters) extends ParameterizedBundle()(p)
        with HasTopLevelParameters

    class TopIO(implicit p: Parameters) extends BasicTopIO()(p) {

      val mem_axi = Vec(nMemAXIChannels, new NastiIO)
      val mem_ahb = Vec(nMemAHBChannels, new HastiMasterIO)
      val interrupts = Vec(p(NExtInterrupts), Bool()).asInput
      val mmio_axi = Vec(p(NExtMMIOAXIChannels), new NastiIO)
      val mmio_ahb = Vec(p(NExtMMIOAHBChannels), new HastiMasterIO)
      val debug = new DebugBusIO()(p).flip
    }

    
There are 4 major I/O ports coming out of the top-level module:

* **Debug interface (debug)**:
The debug interface can be used to both debug the processor as
it is executing, and to read and write memory. It is slated to repalce the
host interface in the near future.
* **High-performance memory interface (mem_*) **:
Memory requests from the processor comes out the mem_* ports.
Depending on the configuration of the design, these may be visible as
AXI or AHB protocol. The mem_* port(s) uses the same uncore clock, and
is intended to be connected to something on the same chip.
* ** Memory mapped I/O interface (mmio_*) **:
The optional mmio_* interfaces can be used to communicate with devices
on the chip but outside of the rocket-chip boundary. Depending on the
configuration of the design, these may be visible as AXI or AHB.
* ** Interrupts interface (interrupts) **: This interface is used to
deliver external interrupts to the processor core.

Of course, there's a lot more in the actual submodules, but hopefully
this would be enough to get you started with using the Rocket chip
generator. We will keep documenting more about our designs in the
respective README of each submodules, release notes, and even blog
posts. In the mean time, please post questions to the hw-dev mailing
list.

## <a name="how"></a> How should I use the Rocket chip generator?

Chisel can generate code for three targets: a high-performance
cycle-accurate C++ emulator, Verilog optimized for FPGAs, and Verilog
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

### <a name="emulator"></a> 1) Using the high-performance cycle-accurate C++ emulator

Your next step is to get the C++ emulator working. Assuming you have N
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
Chisel generated C++ code.

    $ ls $ROCKETCHIP/emulator/generated-src
    Top.DefaultConfig-0.cpp
    Top.DefaultConfig-0.o
    Top.DefaultConfig-1.cpp
    Top.DefaultConfig-1.o
    Top.DefaultConfig-2.cpp
    Top.DefaultConfig-2.o
    Top.DefaultConfig-3.cpp
    Top.DefaultConfig-3.o
    Top.DefaultConfig-4.cpp
    Top.DefaultConfig-4.o
    Top.DefaultConfig-5.cpp
    Top.DefaultConfig-5.o
    Top.DefaultConfig.cpp
    Top.DefaultConfig.h
    emulator.h
    emulator_api.h
    emulator_mod.h

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

### <a name="fpga"></a> 2) Mapping a Rocket core down to an FPGA

We use Synopsys VCS for Verilog simulation. We acknowledge that using a
proprietary Verilog simulation tool for an open-source project is not
ideal; we ask the community to help us move DirectC routines (VCS's way
of gluing Verilog testbenches to arbitrary C/C++ code) into DPI/VPI
routines so that we can make Verilog simulation work with an open-source
Verilog simulator. In the meantime, you can use the C++ emulator to
generate vcd waveforms, which you can view with an open-source waveform
viewer such as GTKWave.

So assuming you have a working Rocket chip, you can generate Verilog for
the FPGA tools with the following commands:

    $ cd $ROCKETCHIP/fsim
    $ make verilog

The Verilog used for the FPGA tools will be generated in
fsim/generated-src. Please proceed further with the directions shown in
the [README](https://github.com/ucb-bar/fpga-zynq/blob/master/README.md)
of the fpga-zynq repository.

However, if you have access to VCS, you will be able to run assembly
tests and benchmarks with the following commands (again assuming you
have N cores on your host machine):

    $ cd $ROCKETCHIP/fsim
    $ make -jN run

The generated output looks similar to those generated from the emulator.
Look into fsim/output/\*.out for the output of the executed assembly
tests and benchmarks.

### <a name="vlsi"></a> 3) Pushing a Rocket core through the VLSI tools

You can generate Verilog for your VLSI flow with the following commands:

    $ cd $ROCKETCHIP/vsim
    $ make verilog

Now take a look at vsim/generated-src, and the contents of the
Top.DefaultConfig.conf file:

    $ cd $ROCKETCHIP/vsim/generated-src
    Top.DefaultConfig.conf
    Top.DefaultConfig.prm
    Top.DefaultConfig.v
    consts.DefaultConfig.vh
    $ cat $ROCKETCHIP/vsim/generated-src/*.conf
    name MetadataArray_tag_arr depth 128 width 84 ports mwrite,read mask_gran 21
    name ICache_tag_array depth 128 width 38 ports mrw mask_gran 19
    name DataArray_T6 depth 512 width 128 ports mwrite,read mask_gran 64
    name HellaFlowQueue_ram depth 32 width 133 ports write,read
    name ICache_T157 depth 512 width 128 ports rw

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
src/main/scala/Configs.scala. Search for NSets and NWays defined in
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

Towards the end, you can also find that ExampleSmallConfig inherits all
parameters from BaseConfig but overrides the same parameters of
SmallConfig.

Now take a look at fsim/Makefile and vsim/Makefile. Search for the
CONFIG variable. DefaultFPGAConfig is used for the FPGA build, while
DefaultConfig is used for the VLSI build. You can also change the
CONFIG variable on the make command line:

    $ cd $ROCKETCHIP/vsim
    $ make -jN CONFIG=ExampleSmallConfig run-asm-tests

Or, even by defining CONFIG as an environment variable:

    $ export CONFIG=ExampleSmallConfig
    $ make -jN run-asm-tests

This parameterization is one of the many strengths of processor
generators written in Chisel, and will be more detailed in a future blog
post, so please stay tuned.

To override specific configuration items, such as the number of external interrupts,
you can create your own Configuration(s) and compose them with Config's ++ operator

    class WithNExtInterrupts extends Config (nExt: Int) {
      (pname, site, here) => pname match {
      case (NExtInterrupts => nExt)
      }
    } 
    class MyConfig extends Config (new WithNExtInterrupts(16) ++ new DefaultSmallConfig)

Then you can build as usual with CONFIG=MyConfig.

## <a name="contributors"></a> Contributors

- Scott Beamer
- Henry Cook
- Yunsup Lee
- Stephen Twigg
- Huy Vo
- Andrew Waterman

## <a name="attribution"></a> Attribution

If used for research, please cite Rocket Chip by the technical report:

Krste Asanović, Rimas Avižienis, Jonathan Bachrach, Scott Beamer, David Biancolin, Christopher Celio, Henry Cook, Palmer Dabbelt, John Hauser, Adam Izraelevitz, Sagar Karandikar, Benjamin Keller, Donggyu Kim, John Koenig, Yunsup Lee, Eric Love, Martin Maas, Albert Magyar, Howard Mao, Miquel Moreto, Albert Ou, David Patterson, Brian Richards, Colin Schmidt, Stephen Twigg, Huy Vo, and Andrew Waterman, _[The Rocket Chip Generator](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html)_, Technical Report UCB/EECS-2016-17, EECS Department, University of California, Berkeley, April 2016
