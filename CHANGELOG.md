WIP -- playing around with what this would look like for a 2020.05.0 release

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

This project adheres to the versioning scheme described in [VERSIONING.md]

## [Unreleased]

### Added
 - Framework for performing Chisel Linting: Truncated assignments and unnamed registers (#2435)
 - TLBroadcast and TLSourceShrinker pass through user bits (#2446)
 - subsystem: add an optional node to use for assigning hartids (#2447)
 - Debug SBA drives AMBA Prot Bits (#2448)
 - AXIMasterPortParameters can carry resources as well (#2443)
 - PRCI Clock[Inward,Outward]Node types (#2441)
 - Definitions for ratified version of RISC-V Processor Trace spec (#2439)
 - Support PLIC elaboration when nDevices == 0 (#2351)
 - Add a registered option to the BundleBroadcast (#2431)
 - Object Model: add Sv48 to RocketISA addressTranslationModes (#2434)


### Changed
  - Update README_TRAVIS.md instrutions for simpler cache maintenance (#2455)
  - Added an assertion that cease output from tiles is monotonic (#2456, #2420)
  - Corrected Travis caching behavior for Scala (CI) (#2454)
  - Use non-sudo Travis container (CI) (#2451)
  - chisel3 version bumped to 3.3.0 (#2399)
  - firrtl version bumped to 1.3.0 (#2399)
  - registers driving cease and wfi have reset values (#2449)
  - Object Model changed for Port protocols (#2444)
  - AddressAdjuster and RegionReplicator now work on prefixes instead of chip ID (multi-chip routing) (#2430)
  - Use static priority arbitration in PTW (#2433)
  - Pipeline tile hartid input for better phyiscal placement (#2432)
  



### Removed
  - package imports that were causing warnings (#2442)


## 2020.04.0
* | | | | | 662921b - Diplomatic tilelink "v1" parameter fixes (#2428) (2 weeks ago) <Henry Cook>
* | | | | |   2d3f5b6 - Merge pull request #2423 from chipsalliance/coreMonWrenFloat (3 weeks ago) <John Ingalls>
* | | | | 0dbe71c - pin Wit version to branch v0.12.0 (#2426) (3 weeks ago) <John Ingalls>
* | | |   3f7074f - Merge pull request #2424 from chipsalliance/stage-ordering-fixes (3 weeks ago) <David Biancolin>
* | | |   4689a06 - Merge pull request #2422 from chipsalliance/supervisor-sans-mmu (3 weeks ago) <Andrew Waterman>
* | |   37fc327 - Merge pull request #2327 from chipsalliance/config-topology (3 weeks ago) <Henry Cook>
* | | |   beb1c71 - Merge pull request #2419 from chipsalliance/deglitch-cease (3 weeks ago) <Andrew Waterman>
* | | | 56dc56e - Bump Chisel and FIRRTL along 3.2.x and 1.2.x (#2417) (3 weeks ago) <Jack Koenig>
* | |   34cf255 - Merge pull request #2410 from chipsalliance/attach-TLError-logicalTreeNodes (3 weeks ago) <Albert Chen>
| * | |   bf52274 - Merge pull request #2411 from chipsalliance/attach-TLError-logicalTreeNodes-mwachs5 (4 weeks ago) <Albert Chen>
* | | 80aa52b - use stages api to build rocket-chip in wake flow (#2413) (3 weeks ago) <Kritik Bhimani>
* | | 9b1907e - print trace log regs only if read or written (#2414) (4 weeks ago) <John Ingalls>
* | |   301854d - Merge pull request #2403 from pentin-as/hardfloat_20200407 (4 weeks ago) <Megan Wachs>
* | | |   7a8b367 - Merge pull request #2412 from chipsalliance/debug-resumeack-race (4 weeks ago) <Ernie Edgar>
* | | | 1bfa2a4 - RTLsim trace log dump the wrong rd register (#2409) (4 weeks ago) <John Ingalls>
* | | |   62edbd8 - Merge pull request #2400 from chipsalliance/vcsr (4 weeks ago) <Andrew Waterman>
* | | |   59c127e - Merge pull request #2364 from seldridge/remove-jgit-resolver (4 weeks ago) <Henry Cook>
* | | | d767038 - Restoring staged generator PRs (Reverting #2329) (#2340) (5 weeks ago) <Deborah Soung>
