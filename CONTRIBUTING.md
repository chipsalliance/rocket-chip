Contributing to Rocket Chip
=====================

Thank you for your interest in contributing to Rocket Chip!

## Table of Contents

+ [Contributing](#submitting)
+ [Submitting a PR](#submitting)
+ [Conditions for Merging a PR](#merging)
+ [Bumping Submodules](#bumping)
+ [Bumping Chisel and FIRRTL](#bumping-chisel)

### <a name="contributing"></a> Contributing to this repo
This repository is managed by EasyCLA. Project participants must sign the free [CHIPS Alliance Contributer License Agreement](https://github.com/chipsalliance/tsc/tree/main/cla) before making a contribution. You only need to do this one time, and it can be signed by [individual](https://github.com/chipsalliance/tsc/tree/main/cla#sign-as-an-individual) contributors or their [employers](https://github.com/chipsalliance/tsc/tree/main/cla#have-your-company-sign-for-you).

To initiate the signature process please open a PR against this repo. The EasyCLA bot will block the merge if we still need a membership agreement from you.
You can find [detailed information here](https://github.com/chipsalliance/tsc/tree/main/cla). If you have issues, please email [operations@chipsalliance.org](mailto:operations@chipsalliance.org).

If your company benefits from CHIPS Alliance projects and you would like to provide essential financial support for the systems and people that power our community, please also consider [membership in the CHIPS Alliance](https://chipsalliance.org/join/).

### <a name="submitting"></a> Submitting a PR

You do not need write access to this repository to make a PR,
you can fork and make a PR from your forked version.

Please see the Github documentation for [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests).

Please ensure to fill out the PR template appropriately and update it if your PR changes.

### <a name="merging"></a> Conditions for Merging a PR

Currently, the requirements for merging a PR are:
 + At least one approved review from an administrator
 + Passing the GitHub actions checks

### <a name="bumping"></a> Bumping Submodules

Several projects are managed as git submodules.

#### When to bump

Most projects will be bumped by developers as needed; however,
sometimes users may wish to speed up the bumping process.
For Chisel 3 and FIRRTL, please see the instructions in [Bumping Chisel and FIRRTL](#bumping-chisel).

#### How to bump

1. Bump the Git submodule

```bash
# Check out a branch (starting at rocket-chip root
git checkout -b my-bumping-branch

# Check out the commit of the submodule you wish to bump to
cd <submodule>
git checkout <commit>
cd ..
```

2. Commit the changes

```bash
# Add and commit the submodule
git add <submodule>
git commit -m "<meaningful message about bumping>"
```

If you are bumping `Chisel 3` or `FIRRTL`, it is ideal to include some notes about
major feature or performance improvements in your commit message.

3. Open a Pull Request on Github

Please see the Github documentation for [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests)

### <a name="bumping-chisel"></a> Bumping Chisel and FIRRTL

Because Chisel and FIRRTL have mature release processes, Rocket Chip uses the published artifacts.

To bump the published dependencies, bump the versions at the top of the Mill build file: [build.sc](build.sc).
Typically, the SBT dependency will only list a version for Chisel 3 which itself depends on FIRRTL.

