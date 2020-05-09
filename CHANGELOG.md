WIP -- playing around with what this would look like for a 2020.05.0 release

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

This project adheres to the versioning scheme described in [VERSIONING.md]

## [Unreleased]

### Added
 - Framework for performing Chisel Linting: Truncated assignments and unnamed registers (#2435)

### Changed
  - Update README_TRAVIS.md instrutions for simpler cache maintenance (#2455)
  - Added an assertion that cease output from tiles is monotonic (#2456,  )
  - Corrected Travis caching behavior for Scala (#2454)

### Removed


## 2020.04.0


* |   a023a83 - Merge pull request #2451 from chipsalliance/mwachs5-patch-4 (3 days ago) <Megan Wachs>
  
* |   f1037ef - Merge pull request #2446 from chipsalliance/broadcasthub-prot (4 days ago) <Henry Cook>
* |   1872f5d - Merge pull request #2399 from chipsalliance/chisel-3-3 (4 days ago) <Jack Koenig>
* |   97ef34b - Merge pull request #2447 from chipsalliance/optional-hartid-prefix (5 days ago) <Andrew Waterman>
* | | d7bab00 - Provide reset values for registers driving cease and wfi (#2449) (5 days ago) <Sandeep Rajendran>
* | |   fb389e1 - Merge pull request #2448 from chipsalliance/debug-prot-bits (5 days ago) <Ernie Edgar>
* |   3a61029 - Merge pull request #2444 from chipsalliance/ahb_axi_om_update (9 days ago) <Mohit Wani>
* |   7c94e1a - Merge pull request #2440 from chipsalliance/revert-2427-ahb_axi_om_update (10 days ago) <Mohit Wani>
* | | 8bd04c4 - AXIS: allow masters to carry resources as well (#2443) (10 days ago) <Wesley W. Terpstra>
* | | 8ca1143 - Chisel compile warning: Chisel3 package import (#2442) (11 days ago) <John Ingalls>
* | | d327212 - prci: add Clock[Inward,Outward]Node types (#2441) (11 days ago) <Wesley W. Terpstra>
* | |   1888c18 - Merge pull request #2439 from chipsalliance/trace-core-interface-update (11 days ago) <Ernie Edgar>
* | |   fca09ed - Merge pull request #2427 from chipsalliance/ahb_axi_om_update (12 days ago) <Mohit Wani>
* | | |   e1a5b25 - Merge pull request #2420 from chipsalliance/cease-assertion (12 days ago) <Megan Wachs>
* | | | | |   8acb224 - Merge pull request #2430 from chipsalliance/revised-multichip-routing (13 days ago) <Wesley W. Terpstra>
* | | | | | |   0fb17d4 - Merge pull request #2351 from jerryz123/patch-2 (13 days ago) <Megan Wachs>
* | | | | | | 373b847 - BundleBroadcast: add optional register pipelining (#2431) (2 weeks ago) <Wesley W. Terpstra>
* | | | | | |   9002625 - Merge pull request #2434 from chipsalliance/OM_Sv48 (2 weeks ago) <John Ingalls>
* | | | | |   05ea095 - Merge pull request #2433 from chipsalliance/ptw-static-arb (2 weeks ago) <Andrew Waterman>
* | | | | | | 3fc2850 - RocketTile: do not distort placement when hartid is not constant (#2432) (2 weeks ago) <Wesley W. Terpstra>
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
