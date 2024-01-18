Rocket Chip Generator :rocket: ![Build Status](https://github.com/chipsalliance/rocket-chip/workflows/Continuous%20Integration/badge.svg?branch=master)
=====================

This repository contains the Rocket chip generator necessary to instantiate
the RISC-V Rocket Core. For more information on Rocket Chip, please consult our [technical report](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html).

## RocketChip Dev Meeting

RocketChip development meetings happen every 2 weeks on Wednesday 17:00 – 18:00am CST (Pacific Time - Los Angeles) with meeting notes [here](https://docs.google.com/document/d/1NjDnf-i10QE0y-qI94A67uCspDRdCIS_IRTm4jc0Ycc):
- Click [here](https://calendar.google.com/calendar/ical/c_699527d804418f900468a49b413d1f9c08e13c0f3f872ce551fc0470d4cdf983%40group.calendar.google.com/public/basic.ics) to subscribe Meeting Schedule(iCal format)
- Click [here](https://calendar.google.com/calendar/embed?src=c_699527d804418f900468a49b413d1f9c08e13c0f3f872ce551fc0470d4cdf983%40group.calendar.google.com) to view Meeting Schedule via Google Calendar
- Click [here](https://sifive.zoom.us/j/93899365000?pwd=UG1HSFJ4ODFzR2dhMHU2bUNqbXc3Zz09) to join Zoom meeting (ID: 93899365000, passcode: 754340)

For possible time adjustments, they will be negotiated in Slack and published in the calendar.

## Table of Contents

+ [Quick instructions](#quick) for those who want to dive directly into the details without knowing exactly what's in the repository.
+ [What's in the Rocket chip generator repository?](#what)
+ [How should I use the Rocket chip generator?](#how)
    + [Using the cycle-accurate Verilator simulation](#emulator)
    + [Mapping a Rocket core down to an FPGA](#fpga)
    + [Pushing a Rocket core through the VLSI tools](#vlsi)
+ [How can I parameterize my Rocket chip?](#param)
+ [Debugging with GDB](#debug)
+ [Building Rocket Chip with an IDE](#ide)
+ [Contributors](#contributors)

## <a name="quick"></a> Quick Instructions

### Checkout The Code

    $ git clone https://github.com/ucb-bar/rocket-chip.git
    $ cd rocket-chip
    $ git submodule update --init

### Install Necessary Dependencies

You may need to install some additional packages to use this repository.
Rather than list all dependencies here, please see the appropriate section of the READMEs for each of the subprojects:

* [rocket-tools "Ubuntu Packages Needed"](https://github.com/freechipsproject/rocket-tools/blob/master/README.md)
* [chisel3 "Installation"](https://github.com/ucb-bar/chisel3#installation)

### Building The Project

Generating verilog

    $ make verilog

Generating verilog for a specific Config

    $ make verilog CONFIG=DefaultSmallConfig

### Keeping Your Repo Up-to-Date

If you are trying to keep your repo up to date with this GitHub repo,
you also need to keep the submodules and tools up to date.

    $ # Get the newest versions of the files in this repo
    $ git pull origin master
    $ # Make sure the submodules have the correct versions
    $ git submodule update --init --recursive

If rocket-tools version changes, you should recompile and install rocket-tools according to the directions in the [rocket-tools/README](https://github.com/freechipsproject/rocket-tools/blob/master/README.md).

    $ cd rocket-tools
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
* **rocket-tools**
([https://github.com/freechipsproject/rocket-tools](https://github.com/freechipsproject/rocket-tools)):
We tag a version of RISC-V software tools that work with the RTL committed in this repository.
* **torture**
([https://github.com/ucb-bar/riscv-torture](https://github.com/ucb-bar/riscv-torture)):
This module is used to generate and execute constrained random instruction streams that can
be used to stress-test both the core and uncore portions of the design.

### <a name="what_packages"></a>Scala Packages

In addition to submodules that track independent Git repositories,
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
are dynamically negotiated between modules. For more information about diplomacy, see [this paper](https://carrv.github.io/2017/papers/cook-diplomacy-carrv2017.pdf).
* **groundtest**
This RTL package generates synthesizable hardware testers that emit randomized
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
This utility package contains a framework for generateing synthesizable hardware testers of individual modules.
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
* **docs**
Documentation, tutorials, etc for specific parts of the codebase.
* **emulator**
Directory in which Verilator simulations are compiled and run.
* **regression**
Defines continuous integration and nightly regression suites.
* **scripts**
Utilities for parsing the output of simulations or manipulating the contents of source files.
* **vsim**
Directory in which Synopsys VCS simulations are compiled and run.
* **vsrc**
Verilog sources containing interfaces, harnesses and VPI.

## <a name="ide"></a> IDEs Support

The Rocket Chip Scala build uses [mill](https://github.com/com-lihaoyi/mill) as build tool.

IDEs like [IntelliJ](https://www.jetbrains.com/idea/) and [VSCode](https://code.visualstudio.com/) are popular in the Scala community and work with Rocket Chip.

The Rocket Chip currently uses `nix` to configure the build and/or development environment, you need to install it first depending on your OS distro.

Then follow the steps:

1. Generate BSP config by running:

   ```
   mill mill.bsp.BSP/install
   ```

2. Patch the `argv` in `.bsp/mill-bsp.json`, from

   ```json
   {"name":"mill-bsp","argv":["/usr/bin/mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.10.9","bspVersion":"2.0.0","languages":["scala","java"]}
   ```

   to

   ```json
   {"name":"mill-bsp","argv":["/usr/bin/nix","develop","-c","mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.10.9","bspVersion":"2.0.0","languages":["scala","java"]}
   ```
   
### For IntelliJ users

3. Install and configure [Scala](https://plugins.jetbrains.com/plugin/1347-scala) plugin.

4. BSP should be automatically run.
   If it doesn't, click `bsp` on the right bar, then right-click on your project to reload.

### For VSCode users

3. Install and configure [Metals](https://marketplace.visualstudio.com/items?itemName=scalameta.metals) extension.

4. Execute VSCode command `Metals: Import build`.

## <a name="contributors"></a> Contributors

Contributing guidelines can be found in [CONTRIBUTING.md](CONTRIBUTING.md).

A list of contributors can be found [here](https://github.com/chipsalliance/rocket-chip/graphs/contributors).

## <a name="attribution"></a> Attribution

If used for research, please cite Rocket Chip by the technical report:

Krste Asanović, Rimas Avižienis, Jonathan Bachrach, Scott Beamer, David Biancolin, Christopher Celio, Henry Cook, Palmer Dabbelt, John Hauser, Adam Izraelevitz, Sagar Karandikar, Benjamin Keller, Donggyu Kim, John Koenig, Yunsup Lee, Eric Love, Martin Maas, Albert Magyar, Howard Mao, Miquel Moreto, Albert Ou, David Patterson, Brian Richards, Colin Schmidt, Stephen Twigg, Huy Vo, and Andrew Waterman, _[The Rocket Chip Generator](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-17.html)_, Technical Report UCB/EECS-2016-17, EECS Department, University of California, Berkeley, April 2016
