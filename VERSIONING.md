Rocket Chip Versioning Strategy ** All Strawman to be discussed**
===============================

Version Naming
--------------

* Rocket Chip uses a Calendar Versioning release strategy (https://calver.org/).
* The format is YYYY.MM.DD.MINOR
* MINOR changes include:
  * API Additions
  * Bug Fixes (which can result in changes in the emitted hardware considered bugs)
* MINOR changes do NOT include:
  * Changes in top-level I/Os for < level of hierarchy >
  * Backwards incompatible changes in the API (see below)

API
---

The Rocket-Chip API is: 
  * All public Scala methods and classes
  * All public Scala methods and classes of included Scala libraries (hardfloat, CDE, etc)
  * The command line used to invoke the generator
  * All Verilog interfaces (I/Os and parameterization) for verilog files
  * C code...?
  * The version(s) of Chisel, FIRRTL, rocket-tools pointed to by rocket-chip.
  * Any code that generates IO(s) of any Module
  * The cmd line interface to scripts like vlsi-mem-gen
  * `public val`s within `LazyModule`s which translate to diplomatic nodes (?? -- this could go either way)

The following is NOT part of the API:
  * `public val`s within `Module`s which translate to Wires, Regs, module instances
    * (We note that this may cause issues for people using Cross-Module references)
  * Internals of scripts like vlsi-mem-gen, the contents of such scripts
  * Internals of any verilog black boxes
  * C Code ...?
      
Roadmap
-------

* Upcoming Versions:

  - Version ~2020.04.04.0: Released approximately before the breaking changes discussed for version NEXT:
  - Version NEXT YY.MM.DD.0: To be released after:
    - Chisel is bumped to 3.3
    - FIRRTL is bumped to X.XX
    - Command line interface updated for staged generator
    
* Following versions released quarterly

* We attempt to deprecate APIs gracefully over at least 2 quarterly releases, but do not guarantee this.

Previous Versions
-----------------

Many 1.2.x versions have been released previously as needed.
