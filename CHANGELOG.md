
# Changelog

## Future Release

Some highlights to be included in the next release:

* Bump to chisel 3.5.4 (https://github.com/chipsalliance/rocket-chip/pull/3105)

* As Chisel compatability layer is being sunset, update chisel2 legacy code to chisel3 (https://github.com/chipsalliance/rocket-chip/pull/3097)

* Removal of `RocketTilesKey` and `RocketCrossingKey` (https://github.com/chipsalliance/rocket-chip/pull/3133)

## Version 1.6.0

10 Oct 2022

### Added
* Bump to Chisel 3.5.3 (https://github.com/chipsalliance/rocket-chip/pull/2931, https://github.com/chipsalliance/rocket-chip/pull/2937, https://github.com/chipsalliance/rocket-chip/pull/2947, https://github.com/chipsalliance/rocket-chip/pull/3005)
* Support for Scala 2.12.15 (https://github.com/chipsalliance/rocket-chip/pull/2947)
* Properly-sized don't cares for FPU typeTag fields (https://github.com/chipsalliance/rocket-chip/pull/2949)
* Add a `virtual` argument to `TLBEntry.sectorHit` function (https://github.com/chipsalliance/rocket-chip/pull/2952)
* Support building PTW with no PTECache (https://github.com/chipsalliance/rocket-chip/pull/2962)
* Update `IncoherentBusTopology` to support multiclock and custom clocking (https://github.com/chipsalliance/rocket-chip/pull/2940)
* Allow forcing `RocketTiles` into separate PRCI groups (https://github.com/chipsalliance/rocket-chip/pull/2842)
* Add a `WithHypervisor` config (https://github.com/chipsalliance/rocket-chip/pull/2946)
* Add ScalaDoc documentation for I$ (https://github.com/chipsalliance/rocket-chip/pull/3001)

### Changed
* AHPParameters and APBParameters:
    * `PROT_PRIVILEDGED` - This was a typo. It is now `PROT_PRIVILEGED`. (https://github.com/chipsalliance/rocket-chip/pull/2925)
* GrountTestTile: use generic `BuildHellaCache` key (https://github.com/chipsalliance/rocket-chip/pull/2919)
* copy EICG wrapper from vsrc when using Clock Gate Model  (https://github.com/chipsalliance/rocket-chip/pull/2969/)
* PTW page fault instead of access exception if PTE reserved bit set (https://github.com/chipsalliance/rocket-chip/pull/2913, https://github.com/chipsalliance/rocket-chip/pull/2934)
    * `PTE_RSVD` was introduced into Spike in riscv-software-src/riscv-isa-sim#750
    * Reserved PTE bits report page fault instead of access exception.
    * Add an additional bit `pf` to `PTWResp` and `TLBEntryData` to pipe this through.
* Have HFENCE.GVME `sfence.bits.hg=1, hv=0` only target TLB entries with V=1 (and not V=0) (https://github.com/chipsalliance/rocket-chip/pull/2954)
* Update Instructions from riscv-opcodes and separate out rocket-specific custom instructions (https://github.com/chipsalliance/rocket-chip/pull/2956, https://github.com/chipsalliance/rocket-chip/pull/2972)
    * post riscv-opcode instruction category scheme at this PR https://github.com/riscv/riscv-opcodes/pull/106
* Decode: switch to using Chisel Decode API (https://github.com/chipsalliance/rocket-chip/pull/2836, https://github.com/chipsalliance/rocket-chip/pull/2994)
* Convert `toaxe.py` to python3 (https://github.com/chipsalliance/rocket-chip/pull/3034)
* make `AsyncClockGroupsKey` a node generator (https://github.com/chipsalliance/rocket-chip/pull/2935)
* change `debug` module name to `tlDM` (https://github.com/chipsalliance/rocket-chip/pull/3029)
* As part of a larger migration begin refactoring files to chisel3:
    * ALU.scala (https://github.com/chipsalliance/rocket-chip/pull/3039)
    * AMOALU.scala (https://github.com/chipsalliance/rocket-chip/pull/3040)
    * Breakpoint.scala (https://github.com/chipsalliance/rocket-chip/pull/3041)
    * PMP.scala (https://github.com/chipsalliance/rocket-chip/pull/3042)
    * FormalUtils.scala (https://github.com/chipsalliance/rocket-chip/pull/3044)
    * Decode.scala (https://github.com/chipsalliance/rocket-chip/pull/3049)
    * Consts.scala (https://github.com/chipsalliance/rocket-chip/pull/3052)

### Fixed
* Proper translation to HRProt3 in AHB Protocol (https://github.com/chipsalliance/rocket-chip/pull/2928/)
* Assert HasFSDirty false (https://github.com/chipsalliance/rocket-chip/pull/2997)
* VSStatus is now read-only and dirty when RoCC is enabled (https://github.com/chipsalliance/rocket-chip/pull/2984)
* RocketCore: avoid false RAW/WAW hazards for integer instructions using an `x` register whose numeric specifier coincides with a previous instruction's `f` register. (https://github.com/chipsalliance/rocket-chip/pull/2945)
* Correct `rocc_illegal` to use `reg_vsstatus.xs` field (https://github.com/chipsalliance/rocket-chip/pull/2983)
* Zero out `aux_pte.reserved_for_future` whenever `aux_pte.ppn` is driven (https://github.com/chipsalliance/rocket-chip/pull/3003)
* Prevent ILTB miss fault PTW thrashing D$ (https://github.com/chipsalliance/rocket-chip/pull/3004)
* Prevent nonsensical use of RVE with Hypervisor (https://github.com/chipsalliance/rocket-chip/pull/2988)
* Explicity outline Rocket's lack of support for `haveFSDirty` (https://github.com/chipsalliance/rocket-chip/pull/2997)
* Fix bit-width out of range issue when both Sv57 and Hypervisor are enabled (https://github.com/chipsalliance/rocket-chip/pull/3006)
* Fix synthesizability of `RoccBlackBox` with Vivado (https://github.com/chipsalliance/rocket-chip/pull/3035)

### Removed
* Remove Object Model from Diplomacy (https://github.com/chipsalliance/rocket-chip/pull/2967/)
* Removed RegEnable explicit arguments in preparation for changes in Chisel 3.6 (https://github.com/chipsalliance/rocket-chip/pull/2986)
* Removed all mentions of Travis CI and .travis.yml file (https://github.com/chipsalliance/rocket-chip/pull/2647)
* Remove `TraceGen` from `HeterogeneousTileExampleConfig (https://github.com/chipsalliance/rocket-chip/pull/2923)


## Version 1.5.0

18 Jan 2022

### Added
* Chisel 3.5 support
* Hypervisor extension (https://github.com/chipsalliance/rocket-chip/pull/2841):
    * Introduce virtualization of hart id for virtualized supervior OSs and virtualized user modes
    * More work to be done on interrupt controllers, IOMMUs, etc.
* Fault if reserved bits D/A/U of page table entry are set to 1 (https://github.com/chipsalliance/rocket-chip/pull/2895)
* `mnie` bit added to mnstatus (https://github.com/chipsalliance/rocket-chip/pull/2904)
* `WithCoherentBusTopology` added to `BaseFPGAConfig` (https://github.com/chipsalliance/rocket-chip/pull/2787)
* Add support for timebase-frequency in the cpus node of the Device Tree (https://github.com/chipsalliance/rocket-chip/pull/2782)
* Support incoherent access to `ExtMem` Bus through SBus (https://github.com/chipsalliance/rocket-chip/pull/2978)

### Changed
* TLToAXI4: b-channel acks are now stalled for if blocked for 7 consecutive cycles (https://github.com/chipsalliance/rocket-chip/pull/2805)
* Cacheable ROMs: treat acquire-able read-only memory as cacheable (https://github.com/chipsalliance/rocket-chip/pull/2808)
* Modularly wrap the value of `nextSelectEnc` in the `ReadyValidCancelRRArbiter` when Round-Robin parameters are `rr=true && !isPow2(n)` (https://github.com/chipsalliance/rocket-chip/pull/2798)
* Changed TLMonitor to check the correct opcode for a (so far unused) B channel Get message (https://github.com/chipsalliance/rocket-chip/pull/2788)
* D$: drive Tilelink C-Channel AMBA_PROT bits last (https://github.com/chipsalliance/rocket-chip/pull/2770/)

### Fixed
* PTW and TLB fault prioritization:
    * Misaligned faults (https://github.com/chipsalliance/rocket-chip/pull/2926):
        * We cannot check if the memory address has side effects to take a misaligned exception if a PTW doesn't finish within a valid PTE.
        * Therefore, misaligned faults are now given lowest priority.
    * Access exceptions (https://github.com/chipsalliance/rocket-chip/pull/2916):
        * Separate access faults into faults for accessing Page Table Entries and faults for bad Physical Page Numbers.
* Hoist `r_valid_vec` onto a register before L2TLB refill `wmask` (https://github.com/chipsalliance/rocket-chip/pull/2868 https://github.com/chipsalliance/rocket-chip/pull/2856)
* Dedpulicate to one OptimizationBarrier per TLBEntry (https://github.com/chipsalliance/rocket-chip/pull/2833)
* D$: block until ReleaseAck from slave acknowledging completion of writeback (https://github.com/chipsalliance/rocket-chip/pull/2832)
* dtim: convert PutPartials to PutFulls when mask is full to avoid RMW (https://github.com/chipsalliance/rocket-chip/pull/2822)
* dtim: don't let `dmem.req.bits.cmd` become X which causes X-prop (https://github.com/chipsalliance/rocket-chip/pull/2818)
* Jam WidthWidget until write to prevent leaking of X output (https://github.com/chipsalliance/rocket-chip/pull/2815)
* Don't cover non-existent U-mode counters (https://github.com/chipsalliance/rocket-chip/pull/2817)
* Supress SCIE assertion when instruction not valid (https://github.com/chipsalliance/rocket-chip/pull/2816)
* Fixed an issue where store fails to take effect if it is immediately followed by a load to the same address under an ECC error condition (https://github.com/chipsalliance/rocket-chip/pull/2804)
* ReadyValidCancelRRArbiter: fixed an issue where round-robin select rotated incorrectly when `rr=true` (https://github.com/chipsalliance/rocket-chip/pull/2771)
* TraceGen: now observes `dmem.ordered` when attempting a fence (https://github.com/chipsalliance/rocket-chip/pull/2779)

### Removed
* Remove unrecoverable non-maskable interrupts (https://github.com/chipsalliance/rocket-chip/pull/2904)
* Remove wake support (https://github.com/chipsalliance/rocket-chip/pull/2847)
* Remove ability to build Chisel/FIRRTL from source

### Additional Notes
* regmapper: update all regmap tests
* tilelink: buswrapper leave fromPort

## Version 1.4.0

19 Dec 2020

RC has undergone two years of development since the last version update. The changelog for this version of RC is non-extensive.
The changelog for this version is merely illustrative of the features added since the 1.2~1.3 releases. No API compatibility is guaranteed between minor version releases of RC.
Future versions of the changelog should follow the format here https://keepachangelog.com/en/1.0.0/

* Chisel 3.4.x and FIRRTL 1.4 compatible.
  * Rely on building from source by default (https://github.com/chipsalliance/rocket-chip/pull/2617)
  * bump to 3.4.0 and FIRRTL 1.4.0 (https://github.com/chipsalliance/rocket-chip/pull/2694)
* Verilator 4.028 compatible (https://github.com/chipsalliance/rocket-chip/pull/2377)
* submodules
  * torture https://github.com/ucb-bar/riscv-torture/tree/77195ab12aefc373ca688e0a9c4d710c13191341
  * hardfloat https://github.com/ucb-bar/berkeley-hardfloat/tree/01904f99ed3ad26cdbe2876f638d63e30e7fecdc
  * cde https://github.com/chipsalliance/cde/tree/fd8df1105a92065425cd353b6855777e35bd79b4
  * if building from source for firrtl and chisel:
    * firrtl: https://github.com/chipsalliance/firrtl/tree/7756f8f9634b68a1375d2c2ca13abc5742234201
    * chisel: https://github.com/chipsalliance/chisel3/tree/58d38f9620e7e91e4668266686484073c0ba7d2e
* scala 2.12.10
* scalatest 3.2.0
* json4s-jackson 3.6.1

### PR Release Notes

#### Rocket
* [CSR] add vcsr and move vxrm/vxstat from fcsr to that register set (https://github.com/chipsalliance/rocket-chip/pull/2400, https://github.com/chipsalliance/rocket-chip/pull/2422)
* [CSR] disallow writes to MSTATUS.XS (https://github.com/chipsalliance/rocket-chip/pull/2508)
* [CSR] expand TracedInstruction.cause to xLen (https://github.com/chipsalliance/rocket-chip/pull/2548)
* [CSR][mstatus] implement updated MPRV from priv-1.12 (https://github.com/chipsalliance/rocket-chip/pull/2206)
* [CSR] add `mcountinhibit from priv-1.11 (https://github.com/chipsalliance/rocket-chip/pull/2693)
  * ignore PAUSE when `mcountinhibit(0)` === 1 (https://github.com/chipsalliance/rocket-chip/pull/2700)
* [CSR] Comply with priv spec by resetting and initializing mcause to 0 (https://github.com/chipsalliance/rocket-chip/pull/2333)
* [events] add SuperscalarEventSets (https://github.com/chipsalliance/rocket-chip/pull/2337, https://github.com/chipsalliance/rocket-chip/pull/2506)
* [events] make fields public for tapping signals (https://github.com/chipsalliance/rocket-chip/pull/2464, https://github.com/chipsalliance/rocket-chip/pull/2524)
* [i$] fix ccover bug to cover all beats of D channel corruption https://github.com/chipsalliance/rocket-chip/pull/2755
* [d$] updates
  * fix elaboration with < 4 MiB of physical address space (https://github.com/chipsalliance/rocket-chip/pull/2367)
  * guarantee no-alloc accesses are ordered even if aliased (https://github.com/chipsalliance/rocket-chip/pull/2358)
  * [ecc] fixed a rare bug where under the right conditions stores to the same word resulted in one store detecting an error while the other does not (https://github.com/chipsalliance/rocket-chip/pull/2458)
  * [HellaCache] introduce `subWordBits` param to support subbanking (https://github.com/chipsalliance/rocket-chip/pull/2645)
  * support specifying cache index when aliasing is possible (https://github.com/chipsalliance/rocket-chip/pull/2697, https://github.com/chipsalliance/rocket-chip/pull/2730)
  * reduce latency on inclusion and coherence misses by allowing D$ to voluntarily release (aka "noisy drop") cache lines (https://github.com/chipsalliance/rocket-chip/pull/2696)
    * follow-up to fix deadlock (https://github.com/chipsalliance/rocket-chip/pull/2714)
    * follow-up to fix performance (https://github.com/chipsalliance/rocket-chip/pull/2739)
* distinguish a supervisor mode that does not use MMU/VM (https://github.com/chipsalliance/rocket-chip/pull/2422, https://github.com/chipsalliance/rocket-chip/pull/2499)
* [hartid]
  * fixed an issue where the Rocket core's placement would be impacted by non-constant hartid (https://github.com/chipsalliance/rocket-chip/pull/2432)
  * add a diplomatic node for assigning hartid (https://github.com/chipsalliance/rocket-chip/pull/2447)
* [Replacement][PseudoLRU] fix performance issue with PseudoLRU for replacements when number of ways is not a power of 2 (https://github.com/chipsalliance/rocket-chip/pull/2493, https://github.com/chipsalliance/rocket-chip/pull/2498)
* [Replacement][d$] configure replacement policy with parameter to indicate wheteher policy is used on a per-set basis or a global basis (https://github.com/chipsalliance/rocket-chip/pull/2656)
* [PTW]
  * replace round robin arbitration with static arbitration (https://github.com/chipsalliance/rocket-chip/pull/2433)
  * fixed a bug where an L2TLB write would almost always block the next L2TLB search when MMU and clock gating were enabled (https://github.com/chipsalliance/rocket-chip/pull/2601)
  * wait for L2TLB to refill before searching (https://github.com/chipsalliance/rocket-chip/pull/2619)
  * [PTWPerfEvents] add (unused) Performance Monitor Events for L2TLB hit and PTE Cache Miss/Hit (https://github.com/chipsalliance/rocket-chip/pull/2668, https://github.com/chipsalliance/rocket-chip/pull/2688, https://github.com/chipsalliance/rocket-chip/pull/2692)
  * enable configurable set-associtive L2 TLB (https://github.com/chipsalliance/rocket-chip/pull/2748, https://github.com/chipsalliance/rocket-chip/pull/2753)
    * default configuration is direct-mapped
* enable Sv48 setting page levels equal to 4 (https://github.com/chipsalliance/rocket-chip/pull/2434)
* [PMP] remove NA4 coverpoint for pmp granularity > 4 (https://github.com/chipsalliance/rocket-chip/pull/2625)
* [TLB]
  * check PutPartial support separately from PutFull (https://github.com/chipsalliance/rocket-chip/pull/2503)
  * fix a rare refill/invalidate race condition (https://github.com/chipsalliance/rocket-chip/pull/2534)
  * configure L1 D/I TLBs by set, entry, and replacement policy (https://github.com/chipsalliance/rocket-chip/pull/2574, https://github.com/chipsalliance/rocket-chip/pull/2621)
  * add params nTLBBasePageSectors and nTLBSuperpages for both I and D TLBs (https://github.com/chipsalliance/rocket-chip/pull/2595)
* [CoreMonitor]
  * add privilege mode and exception signals (https://github.com/chipsalliance/rocket-chip/pull/2387)
  * now prints only retired instructions (https://github.com/chipsalliance/rocket-chip/pull/2372)
  * separate wren into wrenx/wrenf for integer/float (https://github.com/chipsalliance/rocket-chip/pull/2423/)
  * Add [CoreMonitorBundle] for [FPU] floating point registers (https://github.com/chipsalliance/rocket-chip/pull/2538, https://github.com/chipsalliance/rocket-chip/pull/2541, https://github.com/chipsalliance/rocket-chip/pull/2546, https://github.com/chipsalliance/rocket-chip/pull/2589)
* [FPU] Zfh extension, option for Half-Precision unit (https://github.com/chipsalliance/rocket-chip/pull/2723)
  * replaces `singleIn` and `singleOut` with `typeTagIn` and `typeTagOut`
* preliminary RV32Zfh extension support (https://github.com/chipsalliance/rocket-chip/pull/2359)
* [RVV] -> 0.9 -> 1.0 (https://github.com/chipsalliance/rocket-chip/pull/2477, https://github.com/chipsalliance/rocket-chip/pull/2484, https://github.com/chipsalliance/rocket-chip/pull/2396, https://github.com/chipsalliance/rocket-chip/pull/2552, https://github.com/chipsalliance/rocket-chip/pull/2576)
  * Fractional LMUL
  * Tail-agnostic/mask-agnostic bits
  * EEW loads/stores
  * Some encoding changes
  * tighten fractional LMUL-SEW constraint
  * Instructions: add new and update RISC-V vector extension opcodes
  * reorder fields in vtype
* add B extension opcodes and object model description (https://github.com/chipsalliance/rocket-chip/pull/2678)
* fixed an issue where multiplierIO was unclonable (https://github.com/chipsalliance/rocket-chip/pull/2331)



#### Devices
* [PLIC] add support for PLIC elaboration even when nDevices == 0 (https://github.com/chipsalliance/rocket-chip/pull/2351)
* [PLIC] fix off-by-one for priority register description (https://github.com/chipsalliance/rocket-chip/pull/2718)
* [BuildInDevices] introduce case class parameters to Zero and Error device (https://github.com/chipsalliance/rocket-chip/pull/2684)
  * make instantiation of buffers optional
  * allow for optional instantiation of CacheCork
* [BasicBusBlocker] convert to chisel3, add scala-doc, add factory companion object (https://github.com/chipsalliance/rocket-chip/pull/2630)
* [PhysicalFilter] added scaladoc and `RegFieldDesc` (https://github.com/chipsalliance/rocket-chip/pull/2685)
* [BEU]
  * added a Device Tree description for the bus error unit (https://github.com/chipsalliance/rocket-chip/pull/2373)
  * report Corrupt+Denied on I-Fetch (https://github.com/chipsalliance/rocket-chip/pull/2482)

#### PRCI
* [ResetSynchronizer][ClockGroupResetSynchronizer] add a pair of diplomatic reset synchronizers (https://github.com/chipsalliance/rocket-chip/pull/2666)
  * replaced IdentityNodes with AdapterNodes (https://github.com/chipsalliance/rocket-chip/pull/2689)
* wrap Tiles in PRCI Domains (https://github.com/chipsalliance/rocket-chip/pull/2550)
  * contains logic related to  power, reset, clock, and interrupt
* define `ResetCrossingType` and use with `BlockDuringReset` in `TilePRCIDomain` (https://github.com/chipsalliance/rocket-chip/pull/2641/)
  * analogous to `ClockCrossingType`. Currently, there are two crossing types: `NoResetCrossing` and `StretchedResetCrossing(cycles: Int)`
  * introduces `Blockable` util
* Synchronizer primitive changes (https://github.com/chipsalliance/rocket-chip/pull/2212)
  * introduction of `ClockCrossingReg`
  * _SynchronizerShiftReg requires synchronizer depth > 1
  * deprecate IntXing and IntSyncCrossingSink
  * deprecate SyncResetSynchronizerShiftReg
* [SynchronizerPrimitiveShiftReg] correct the dedup behavior for the *ResetSynchronizerPrimitiveShiftReg so you only end up with one copy (https://github.com/chipsalliance/rocket-chip/pull/2547)
* add partial multiple reset scheme support (https://github.com/chipsalliance/rocket-chip/pull/2375)
* AsyncResetReg: use chisel3 async resets (https://github.com/chipsalliance/rocket-chip/pull/2397)
* Async Reset support for Atomics, FPU, and TLBroadcast (https://github.com/chipsalliance/rocket-chip/pull/2362)
* [ResetStretcher][PRCI] add reset stretcher for Async Reset systems (https://github.com/chipsalliance/rocket-chip/pull/2566)
* ClockGroupDriverParameters: allow for a configurable drive function for driving asynchronous clock groups with IO other than the implicit clock (https://github.com/chipsalliance/rocket-chip/pull/2319)
* [ClockDivider] fixed bug where clock divider's source and sink functions always divided by two (https://github.com/chipsalliance/rocket-chip/pull/2610)
* [InterruptBusWrapper] update synchronizer API (https://github.com/chipsalliance/rocket-chip/pull/2640)
  * replaces using `IntXing` in a `synchronize` method with `to` and `from` methods
  * this is to ensure synchronized registers are always put in the destination clock domain

#### Tile
* [notification] provide reset values for cease and wfi (https://github.com/chipsalliance/rocket-chip/pull/2449)
* [notification][CSR] Block wfi, halt, cease, and other valid signals during asynchronous reset (https://github.com/chipsalliance/rocket-chip/pull/2611)
  * trace.valid of CSR changed to async-reset delay (https://github.com/chipsalliance/rocket-chip/pull/2613)
* [notification][WFI] expose WFI from core (https://github.com/chipsalliance/rocket-chip/pull/2315)
* [i$] fixed bug where cease signal was asserted before potential glitching in I$ clock finished. Add an assertion to cease signal. (https://github.com/chipsalliance/rocket-chip/pull/2419/, https://github.com/chipsalliance/rocket-chip/pull/2420/, https://github.com/chipsalliance/rocket-chip/pull/2456/)
* [PMP][DTS] add pmp granularity to DTS (https://github.com/chipsalliance/rocket-chip/pull/2661)
* [NMI] introduce non-maskable interrupt implementation (https://github.com/chipsalliance/rocket-chip/pull/2711)
* `val tiles` in trait `HasTiles` is now populated eagerly via the `TilesLocated` Field. (https://github.com/chipsalliance/rocket-chip/pull/2504)

#### Subsystem
* [HasTiles] add seipNode (https://github.com/chipsalliance/rocket-chip/pull/2665)
* Topology changed from static traits to CDE-based configurable runtime (https://github.com/chipsalliance/rocket-chip/pull/2327)
  * `HasHierachicalBusTopology` trait replaced with two config options:
    * `WithCoherentBusTopology`
    * `WithIncoherentBusTopology`
* renamed attachment API to location API (https://github.com/chipsalliance/rocket-chip/pull/2330)
* [BundleBridge] to propagate [TileInputConstants]. ROM attachment changes (https://github.com/chipsalliance/rocket-chip/pull/2521 merged as https://github.com/chipsalliance/rocket-chip/pull/2531)
  * `HasPeripheryBootROM` and `HasPeripheryBootROMModuleImp` are removed and replaced by a call to `BootROM.attach`
  * `BootROMParams` Field is removed and replaced with `BootROMLocated` Field
  * `MaskROMLocated` Field is added
  * `SubsystemExternalResetVectorKey`, `SubsystemExternalHartIdWidthKey` and `InsertTimingClosureRegistersOnHartIds` Fields are added
  * Unused `ResetVectorBits` Field is removed
  * `HasExternallyDrivenTileConstants` bundle mixin is removed
  * `HasResetVectorWire` subsystem trait is removed
  * `HasTileInputConstants` and `InstantiatesTiles` subsystem traits are added
  * `BaseTile` exposes `val hartIdNode: BundleBridgeNode[UInt]` and `resetVectorNode: BundleBridgeNode[UInt]` and these are automatically connected to in `HasTiles`.
  * `rocket.Frontend`, `rocket.ICache`, `rocket.DCache`, `rocket.NDCache` now have `BundleBridgeSink[UInt]` for their reset vector or hartid wire inputs.
    * If you instantiate them manually, i.e. not using the traits e.g. `rocket.HasHellaCache`, you will have to manually connect up those nodes to the aforementioned `BaseTile` nodes.
  * follow up PR - bug fix for HartID and ResetVector width calcluation (https://github.com/chipsalliance/rocket-chip/pull/2543/)
* add HierarchicalLocation to LocationAPI (https://github.com/chipsalliance/rocket-chip/pull/2346/)
* [RocketCrossingParams] relax type of `master` param to `TilePortParamsLike` (https://github.com/chipsalliance/rocket-chip/pull/2634/)
* [Subsystem] Miscellaenous subsystem bus crossing changes (https://github.com/chipsalliance/rocket-chip/pull/2724)
  * introduce keys for bus crossings
  * allow for disabling of DriveClockFromMaster behavior
  * introduce MBus crossing to CoherentBusTopology
* [Subsystem][PLIC] avoid using implicit clock (https://github.com/chipsalliance/rocket-chip/pull/2719)
* Add an optional `TileInputConstant` as an MMIO Address Prefix used in ITIM and DTIM hit calculations (https://github.com/chipsalliance/rocket-chip/pull/2533)
  * follow-up: fix traceCoreNode duplication issue (https://github.com/chipsalliance/rocket-chip/pull/2561)

#### stage, linting, transforms
* [stage] Fix a bug where unserializable RocketTestSuiteAnnotations were being serialized (https://github.com/chipsalliance/rocket-chip/pull/2424)
* [stage] Fix a bug where the desired output file name was being superseded by another phase (https://github.com/chipsalliance/rocket-chip/pull/2424)
* [RocketChipStage] Remove emitVerilog, emitFirrtl, and emitChirrtl methods from RocketChipStage (https://github.com/chipsalliance/rocket-chip/pull/2481)
* [stage] expose Stage's `--target-dir` to Config (https://github.com/chipsalliance/rocket-chip/pull/2725)
* [Transforms][Lint] add `RenameDesiredNames` transform and `LintConflictingModuleNames` Lint rule (https://github.com/chipsalliance/rocket-chip/pull/2452)
  * also adds RenameModulesAspect that can be used to emit name overrides and a LintConflictingModuleNamesAspect to collect DesiredNameAnnotations to be checked by the lint pass.
* [ElaborationArtefactAnnotation] add `ElaborationArtefactAnnotation` - an API similar to `ElaborationArtefacts` (https://github.com/chipsalliance/rocket-chip/pull/2727)
  * this API is for assuring metadata has correct instance paths and signal names
  * allow renames to multiple targets for `MemoryPathToken` (https://github.com/chipsalliance/rocket-chip/pull/2729)

#### Debug
* mcontext and scontext CSRs for breakpoint qualification (https://github.com/chipsalliance/rocket-chip/pull/2588/)
* allow a fast debugger reading dmstatus in a single dminner clock cycle to read the proper value (https://github.com/chipsalliance/rocket-chip/pull/2412)
* fix address sent from DM to SB2TL (https://github.com/chipsalliance/rocket-chip/pull/2559)
* add bus blocker to deny requests to dmInner when dmactive = 0 (https://github.com/chipsalliance/rocket-chip/pull/2205)
* DMIToTL: remove PutPartial (https://github.com/chipsalliance/rocket-chip/pull/2598)
* convert registers and wires from a Regs of Vector to Regs of UInt (https://github.com/chipsalliance/rocket-chip/pull/2597)
* make instantiation of reset synchronizers optional (https://github.com/chipsalliance/rocket-chip/pull/2626)
* allow DM at base address other than 0 (https://github.com/chipsalliance/rocket-chip/pull/2649)
* [Periphery] workaround an autonaming bug with debug (https://github.com/chipsalliance/rocket-chip/pull/2657)
* make `nExtTriggers` a val for compatibility with cloneType (https://github.com/chipsalliance/rocket-chip/pull/2667/)
* [BPWatch] have the watchpoint compare to store or load instruction type for matching (https://github.com/chipsalliance/rocket-chip/pull/2317)

#### AMBA, Tilelink
* combine modifiable and cacheable, add read and write alloc fields (https://github.com/chipsalliance/rocket-chip/pull/2386)
* [AXI4Deinterleaver][AXI4IdIndexer][AXI4UserYanker][TLToAXI4][Anotations] (https://github.com/chipsalliance/rocket-chip/pull/2676)
   * Scala doc
   * Clarifying comments
   * Unify `TLToAXI4` metadata code paths into a single path through `TLtoAXI4IdMap`
   * Make any value of `TLToAXI4.stripBits` other than 0 illegal and stop using it internally.
   * Remove usage of un-consumed `Annotated.idMapping` and delete associated application and annotation class.
* [AXI4Deinterleaver] support asynchronous reset  (https://github.com/chipsalliance/rocket-chip/pull/2479/)
* [AXI4Deinterleaver] add buffer when optimized away (https://github.com/chipsalliance/rocket-chip/pull/2642, https://github.com/chipsalliance/rocket-chip/pull/2652)
* [AXIS] allow masters to carry resources (https://github.com/chipsalliance/rocket-chip/pull/2443)
* [SRAM] accomodate address ranges that require more than 32 bits (https://github.com/chipsalliance/rocket-chip/pull/2491)
* [SRAM] Add public accessors for SRAM modules (https://github.com/chipsalliance/rocket-chip/pull/2646)
* [SimAXIMem] introduce `base` address argument to constructor )https://github.com/chipsalliance/rocket-chip/pull/2628)
* [TLRAM] improved cycle time for designs involving TLRAM (https://github.com/chipsalliance/rocket-chip/pull/2582)
* TLMonitors: formal verification support and additional constraints
  * TLVIP2 (https://github.com/chipsalliance/rocket-chip/pull/2271, https://github.com/chipsalliance/rocket-chip/pull/2505, https://github.com/chipsalliance/rocket-chip/pull/2754, https://github.com/chipsalliance/rocket-chip/pull/2537, https://github.com/chipsalliance/rocket-chip/pull/2573)
  * change `isShrink` `TLPermissions` assertion on C channel to `isReport` (https://github.com/chipsalliance/rocket-chip/pull/2675)
* TLEdge: add require failure messages for TL edges (https://github.com/chipsalliance/rocket-chip/pull/2313/)
* minor tilelink v1 parameter fixes for setName and probe rendering (https://github.com/chipsalliance/rocket-chip/pull/2428)
* [TLParameters] add v2 constructors (https://github.com/chipsalliance/rocket-chip/pull/2532)
* [TLParameters] functions to look at emits parameters (https://github.com/chipsalliance/rocket-chip/pull/2572)
* [Parameters] replace cover function with mincover (https://github.com/chipsalliance/rocket-chip/pull/2571)
* [APBToTL] only assert address alignment when data is ready and valid on a-channel (https://github.com/chipsalliance/rocket-chip/pull/2314)
* [TLBroadcast][TLSourceShrinker][TLCacheCork][SBA][$] Drive or pass through TL user bits (https://github.com/chipsalliance/rocket-chip/pull/2457, https://github.com/chipsalliance/rocket-chip/pull/2448, https://github.com/chipsalliance/rocket-chip/pull/2383, https://github.com/chipsalliance/rocket-chip/pull/2446)
* [TLBroadcast] add API to create Probe filters for Broadcast coherence manager  (https://github.com/chipsalliance/rocket-chip/pull/2509)
* [TLBroadcast] fixed a Generator bug when instantiated with no inner cache (https://github.com/chipsalliance/rocket-chip/pull/2516)
* [TLBroadcast] Add control parameters for control interface (https://github.com/chipsalliance/rocket-chip/pull/2519)
* make it possible to filter with Banked Broadcast Hub (https://github.com/chipsalliance/rocket-chip/pull/2545)
* [TLSourceShrinker] preserve meta data when no shrinkage is required (https://github.com/chipsalliance/rocket-chip/pull/2466)
* [TLFragmenter] ensure Fragmenter raises corrupt signal when raising denied (https://github.com/chipsalliance/rocket-chip/pull/2468)
* [Tilelink][Arbiter][Xbar][ReadyValidCancel] Add new API that replaces `valid` with `earlyValid` and `lateCancel` to fix a timing path for A-channel requests (https://github.com/chipsalliance/rocket-chip/pull/2480, https://github.com/chipsalliance/rocket-chip/pull/2488)
* [TLCacheCork] prevent cache block write size from exceeding read size (https://github.com/chipsalliance/rocket-chip/pull/2527)
* [TLCacheCork] switch CacheCork class to take a case class parameter (https://github.com/chipsalliance/rocket-chip/pull/2684)
  * with backwards compatible constructor in helper object
* [TLBundle] C channel now has same user bits as A channel (https://github.com/chipsalliance/rocket-chip/pull/2632)
  * caches now responsible for driving AMBAProt on C-channel.
* [TLArbiter] add `highestIndexFirst` arbitration policy (https://github.com/chipsalliance/rocket-chip/pull/2587)
* [AHBToTL] retain AHB hrdata even during error response (https://github.com/chipsalliance/rocket-chip/pull/2512)
* [AHBToTL] fix spurious fire of assertion on first cycle (https://github.com/chipsalliance/rocket-chip/pull/2523)
* [CreditedIO] introduce new DecoupledIO interface for credit debit buffers (https://github.com/chipsalliance/rocket-chip/pull/2555)
* [IdMap][IdMapEntry] standardize IdMap and IdMapEntry (https://github.com/chipsalliance/rocket-chip/pull/2483)
  * [AXI4IdIndexer] later fixed a bug with graphml parsing metadata bracketed in "< >" (https://github.com/chipsalliance/rocket-chip/pull/2638)
* [IdMapEntry][OMIdMapEntry] add `maxTransactionsInFlight` field https://github.com/chipsalliance/rocket-chip/pull/2627

#### Diplomacy
  * versioning support for tilelink parameters (https://github.com/chipsalliance/rocket-chip/pull/2320)
  * allow users to access Lazy Module nodes (https://github.com/chipsalliance/rocket-chip/pull/2301)
  * JunctionNodes now support configurable up/down ratio (https://github.com/chipsalliance/rocket-chip/pull/2430)
  * dynamic and remote order: fix QoR in designs with large physical address maps (https://github.com/chipsalliance/rocket-chip/pull/2461)
  * [AddressSet] fix a bug where duplicated AddressSets would cause incorrect widening when unify is called. (https://github.com/chipsalliance/rocket-chip/pull/2502)
  * [LazyModule]
    * mark LazyModules for inlining such as nodes with circuit identity (inputs are outputted unchanged) (https://github.com/chipsalliance/rocket-chip/pull/2579)
      * inline xbar patch: (https://github.com/chipsalliance/rocket-chip/pull/2639)
    * add scaladoc (https://github.com/chipsalliance/rocket-chip/pull/2311)
  * Added more debug info to node requires (https://github.com/chipsalliance/rocket-chip/pull/2577)
  * [Nodes] documentation for Nodes (https://github.com/chipsalliance/rocket-chip/pull/2604)
  * [Nodes] replace `bundleSafeNow` guard with `instantiated` guard (https://github.com/chipsalliance/rocket-chip/pull/2680)
  * tutorial for adder (https://github.com/chipsalliance/rocket-chip/pull/2615)
  * [aop][Select] add Select Library API (https://github.com/chipsalliance/rocket-chip/pull/2674)
  * [DTS] allow node names up to 48 bytes (https://github.com/chipsalliance/rocket-chip/pull/2570)
  * [AddressAdjuster] and RegionReplicator now work on prefixes (not chip id) (https://github.com/chipsalliance/rocket-chip/pull/2430)
      * removes MultiChipMaskKey
  * [AddressAdjuster] patches (https://github.com/chipsalliance/rocket-chip/pull/2470)
    * user can now supply a default local base address for reporting manager address metadata other than the 0th region
    * let local and remote legs have different user bits using <:= operator
    * allow for no fifo ordering on the replicated region
    * more verbose requires
  * [BundleBridge] generalize `BundleBroadcast` into `BundleBridgeNexus` (https://github.com/chipsalliance/rocket-chip/pull/2497)
    * user can now supply input and output functions
  * [BundleBridge] add `SafeRegNext` to `BundleBridgeNexus` to preserve width (https://github.com/chipsalliance/rocket-chip/pull/2520)
  * [BundleBroadcast] add register pipelining argument (https://github.com/chipsalliance/rocket-chip/pull/2431)

#### Object Model
* [OMMemoryMap] require register map to only go to one memory region (https://github.com/chipsalliance/rocket-chip/pull/2496)
* [OMErrorDevice] added to Object Model (https://github.com/chipsalliance/rocket-chip/pull/2410, https://github.com/chipsalliance/rocket-chip/pull/2411)
* added IdRange, IDMap to include source ids in object model (https://github.com/chipsalliance/rocket-chip/pull/2495)
* added L2UTLB entries and memory (https://github.com/chipsalliance/rocket-chip/pull/2606)
* [OMISA] Add OMVectorExtension.vstartALU field (https://github.com/chipsalliance/rocket-chip/pull/2578)
* [RegFieldDesc]
  * add AddressBlocks for RegFieldDesc (https://github.com/chipsalliance/rocket-chip/pull/2437)
  * require RegFieldDesc to match RegEx to limit to a subspect of IP-XACT standard (https://github.com/chipsalliance/rocket-chip/pull/2525)
* updates for AHB and AXI (https://github.com/chipsalliance/rocket-chip/pull/2427)
* add Zfh extension (https://github.com/chipsalliance/rocket-chip/pull/2581)

#### Utilities, QoL, Other
* make RC more tolerant to x-prop (https://github.com/chipsalliance/rocket-chip/pull/2659)
* [util][rotate] fix `rotate` for zero-width wires (https://github.com/chipsalliance/rocket-chip/pull/2663)
* [SimJTAG][SimDTM] fix a verilator bug due to delay statements (https://github.com/chipsalliance/rocket-chip/pull/2635)
* register coverage now generated based on access type (https://github.com/chipsalliance/rocket-chip/pull/2384)
* [BundleMap] improved API for user bits
  * Customizable field unification, Default for bulk assignments, Field and Key class required (https://github.com/chipsalliance/rocket-chip/pull/2318)
  * Use BundleMap for AMBA protocols (https://github.com/chipsalliance/rocket-chip/pull/2326)
  * various bug fixes to TL user fields (https://github.com/chipsalliance/rocket-chip/pull/2335)
* [FixChisel3] Added some scaladoc commentary to the operators :<>, :<=, :=> to explain what they do and the rationale for their creation. (https://github.com/chipsalliance/rocket-chip/pull/2339)
* [util] Add utilities for bitwise shifts by signed shift amounts (https://github.com/chipsalliance/rocket-chip/pull/2477)
* [TLBusWrapper] more stability to internal wire names (https://github.com/chipsalliance/rocket-chip/pull/2515)
* [LazyRoCC] convert LazyRoCC to chisel3 (https://github.com/chipsalliance/rocket-chip/pull/2553)
* [OptimizationBarrier] give the module a name in generated verilog (https://github.com/chipsalliance/rocket-chip/pull/2507)
* add test enable pin to Clock Gate (https://github.com/chipsalliance/rocket-chip/pull/2087)
* [RecordMap] addd as an API for better diplomatic IO naming https://github.com/chipsalliance/rocket-chip/pull/2486
  * used to get easier to follow Clock Group signal names https://github.com/chipsalliance/rocket-chip/pull/2528
* [IDPool] enable ResetAsynchronous Full (https://github.com/chipsalliance/rocket-chip/pull/2568)
* [IDPool] add `lateValid` and `revocableSelect` to shift the deep logic cones from before the `valid/selec` registers to after the `bitmap` register (https://github.com/chipsalliance/rocket-chip/pull/2673, https://github.com/chipsalliance/rocket-chip/pull/2677)
* [IDPool] infer widths (https://github.com/chipsalliance/rocket-chip/pull/2679)
* Make AsyncValidSync a RawModule (https://github.com/chipsalliance/rocket-chip/pull/2352)
* compiler warning fixes (https://github.com/chipsalliance/rocket-chip/pull/2357, https://github.com/chipsalliance/rocket-chip/pull/2356, https://github.com/chipsalliance/rocket-chip/pull/2355, https://github.com/chipsalliance/rocket-chip/pull/2354, https://github.com/chipsalliance/rocket-chip/pull/2353, https://github.com/chipsalliance/rocket-chip/pull/2378, https://github.com/chipsalliance/rocket-chip/pull/2379, https://github.com/chipsalliance/rocket-chip/pull/2380, https://github.com/chipsalliance/rocket-chip/pull/2442, https://github.com/chipsalliance/rocket-chip/pull/2567, https://github.com/chipsalliance/rocket-chip/pull/2757, https://github.com/chipsalliance/rocket-chip/pull/2758)
* [SCIE] fix width mismatch assignment lint warning from VCS (https://github.com/chipsalliance/rocket-chip/pull/2563)
* Initial scalatest flow support and aspect generation (https://github.com/chipsalliance/rocket-chip/pull/2309/, https://github.com/chipsalliance/rocket-chip/pull/2517)
* [linting] add Chisel Linting Framework (https://github.com/chipsalliance/rocket-chip/pull/2435)
* [scalafix] enable scalafix and remove unused imports (https://github.com/chipsalliance/rocket-chip/pull/2648)
  * This requires downstream projects using rocket-chip's build.sbt to enable scalafix.
  * enable LeakingImplicitClassVal (https://github.com/chipsalliance/rocket-chip/pull/2650)
  * enable ProcedureSyntax (https://github.com/chipsalliance/rocket-chip/pull/2651)
* mdoc infrastructure (https://github.com/chipsalliance/rocket-chip/pull/2615)
* [PlusArg]
  * support for no-default PlusArgs and string-valued PlusArgs (https://github.com/chipsalliance/rocket-chip/pull/2453)
  * fix VCS lint warning for plusarg default bit width (https://github.com/chipsalliance/rocket-chip/pull/2558, https://github.com/chipsalliance/rocket-chip/pull/2562)
* decode: improve runtime (https://github.com/chipsalliance/rocket-chip/pull/2462)
* Switch to using Github Actions (https://github.com/chipsalliance/rocket-chip/pull/2465, https://github.com/chipsalliance/rocket-chip/pull/2472, https://github.com/chipsalliance/rocket-chip/pull/2530, https://github.com/chipsalliance/rocket-chip/pull/2536)
* Some Travis changes were made, but travis is dropped in later releases (https://github.com/chipsalliance/rocket-chip/pull/2451, https://github.com/chipsalliance/rocket-chip/pull/2454, https://github.com/chipsalliance/rocket-chip/pull/2455, https://github.com/chipsalliance/rocket-chip/pull/2490)
* Add scalatest to a bucket for regression testing (https://github.com/chipsalliance/rocket-chip/pull/2511)
* RTLSim trace log:
  * fixed an issue where the wrong destination register being dumped in the RTLSim trace log (https://github.com/chipsalliance/rocket-chip/pull/2409/)
  * enhanced log so only registers read or written are printed (https://github.com/chipsalliance/rocket-chip/pull/2414)
* add CONTRIBUTING.md (https://github.com/chipsalliance/rocket-chip/pull/2342, https://github.com/chipsalliance/rocket-chip/pull/2473)
* [wake]
  * add self-location rules (https://github.com/chipsalliance/rocket-chip/pull/2526)
  * fix string interpolation of bootrom path (https://github.com/chipsalliance/rocket-chip/pull/2535)
  * publish location of ivydependencies.json (https://github.com/chipsalliance/rocket-chip/pull/2540)
  * update CI to 0.19.0 (https://github.com/chipsalliance/rocket-chip/pull/2594 superseding https://github.com/chipsalliance/rocket-chip/pull/2584)
  * avoid hardcoded directory path for hardfloat repo https://github.com/chipsalliance/rocket-chip/pull/2633
* [mill] add mill build system (https://github.com/chipsalliance/rocket-chip/pull/2654)
* [sbt] remove jgit-repo resolver (https://github.com/chipsalliance/rocket-chip/pull/2364)

## Version 1.3.0

This version exists as a branch, but seems to be largely synonymous with 1.2. There are no release notes or maintenance for this version.

## Version 1.2.0

There are no existing release notes for this and previous versions.