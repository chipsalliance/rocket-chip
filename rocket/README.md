Rocket Core
===========

Rocket is a 6-stage single-issue in-order pipeline that executes the 64-bit
scalar RISC-V ISA.  Rocket implements an MMU that supports page-based virtual
memory and is able to boot modern operating systems such as Linux.  Rocket
also has an optional IEEE 754-2008-compliant FPU, which implements both
single- and double-precision floating-point operations, including fused
multiply-add.

This repository is not intended to be a self-running repository. To
instantiate a Rocket core, please use the Rocket chip generator found in the
rocket-chip git repository.

The following table compares a 32-bit ARM Cortex-A5 core to a 64-bit RISC-V
Rocket core built in the same TSMC process (40GPLUS). Fourth column is the
ratio of RISC-V Rocket to ARM Cortex-A5. Both use single-instruction-issue,
in-order pipelines, yet the RISC-V core is faster, smaller, and uses less
power.

ISA/Implementation | ARM Cortex-A5 | RISC-V Rocket | R/A
--- | --- | --- | ---
ISA Register Width | 32 bits | 64 bits | 2
Frequency | >1 GHz | >1 GHz | 1
Dhrystone Performance | 1.57 DMIPS/MHz | 1.72 DMIPS/MHz | 1.1
Area excluding caches | 0.27 mm<sup>2</sup> | 0.14 mm<sup>2</sup> | 0.5
Area with 16KB caches | 0.53 mm<sup>2</sup> | 0.39 mm<sup>2</sup> | 0.7
Area Efficiency | 2.96 DMIPS/MHz/mm<sup>2</sup> | 4.41 DMIPS/MHz/mm<sup>2</sup> | 1.5
Dynamic Power | <0.08 mW/MHz | 0.034 mW/MHz | >= 0.4
