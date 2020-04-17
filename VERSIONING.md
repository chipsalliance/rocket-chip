Rocket Chip Versioning Strategy ** All Strawman to be discussed**
===============================

Version Naming
--------------

* Rocket Chip uses a Calendar Versioning release strategy (https://calver.org/).
* The format is YYYY.0M.MINOR
* Example:
  * 2020.04.0 is the release which occurred in April, 2020.
  * 2020.04.1 is a release that could occur at any time, but introduces only MINOR changes over the 2020.04.0 release
  * 2020.06.0 is a release that occurs in June 2020, with MAJOR (API breaking) changes over the 2020.04.X release(s).
* MINOR changes include:
  * API Additions
  * Bug Fixes (which can result in changes in the emitted hardware considered bugs)
* MINOR changes do NOT include:
  * Backwards incompatible changes in the API (see below)

API
---

The Rocket-Chip API is: 
  * All public Scala methods and classes
  * All public Scala methods and classes of included Scala libraries (hardfloat, CDE, etc)
  * The command line used to invoke the generator
  * All Verilog interfaces (I/Os and parameterization) for verilog resources
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
  * Makefiles and Makefile fragments
  
      
Roadmap
-------

* Upcoming Versions:

  - Version 2020.04.0: To be released just before the breaking changes discussed for version NEXT:
  - Version NEXT YYYY.0M.0: To be released after:
    - Chisel is bumped to 3.3
    - FIRRTL is bumped to X.XX
    - Command line interface updated for staged generator
    
* Following versions released quarterly

* We attempt to deprecate APIs gracefully over at least 2 quarterly releases, but do not guarantee this.

Version Branches
-----------------

Development occurs for the next major YYYY.0M.0 release on the `master` branch.
When the YYYY.0M.0 release is made, a protected (no force-pushing) branch is created with the name: `YYYY.0M.X`.
The first MAJOR and subsequent MINOR releases will be made as tagged commits on `YYYY.0M.X branch`.

Naming Your Own Releases
---------------------------


If you would like to fork rocket-chip and release your own derivative versions, we suggest:
  * If you fork and cut a specific release off an existing commit off this repo:
    * Name it as <release>-SNAPSHOT-<git commit hash>
    * For example if you want to make a specific release off commit hash `1234beef` which is a commit after the 2020.04.2 release, call it `2020.04.2-SNAPSHOT-1234beef`
  * If you fork and modify a release branch and want to make a release:
    * Name it as <release>-<identifier>-<arbitrary number with meaning decided by the entity doing the release>
    * For example, if GitHub user `@mwachs5` did this: `2020.04.2-mwachs5-0`
 * This document suggests no requirements/restrictions on the changes made in such forks

Previous Versions
-----------------

Many 1.2.x versions have been released previously as needed.
