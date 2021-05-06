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
 + Passing at least one of the GitHub actions checks or the Travis checks

### <a name="bumping"></a> Bumping Submodules

Several projects are managed as git submodules as well as [Wit](https://github.com/sifive/wit) dependencies.

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

2. Bump the Wit submodule

Update the `commit` field for the specific submodule in `wit-manifest.json`.
You can do this by simply editing the file in your text editor of choice.

**Tip** `git -C <submodule> rev-parse HEAD` will give you the commit hash

3. Commit the changes

```bash
# Add and commit the submodule
git add <submodule>
git add wit-manifest.json
git commit -m "<meaningful message about bumping>"
```

If you are bumping `Chisel 3` or `FIRRTL`, it is ideal to include some notes about
major feature or performance improvements in your commit message.

4. Open a Pull Request on Github

Please see the Github documentation for [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests)

### <a name="bumping-chisel"></a> Bumping Chisel and FIRRTL

Because Chisel and FIRRTL have mature release processes, Rocket Chip uses the published artifacts when possible.
However, Rocket Chip maintains the ability to easily switch to building Chisel and FIRRTL from source.
This support is useful for long-lived tapeout branches and forks that may stuck on old versions of Chisel or FIRRTL with tapeout-specific bugfixes or hacks.

_Whether Rocket Chip is building Chisel and FIRRTL from published artifacts or from source is a property of a given commit_.
Users should *not* be changing this behavior locally; it should only be changed as part of bumping the dependencies.

In order to support switching between building against both source and published version of Chisel 3 and FIRRTL,
Rocket Chip uses an SBT plugin that it configures via JVM system properties.
This flow is described in the [Chisel README](https://github.com/freechipsproject/chisel3#building-chisel-with-firrtl-in-the-same-sbt-project).

When a file named `.sbtopts` exists in the root of this repository, it means Rocket Chip is configured to build Chisel 3 and FIRRTL from source.
When this file does not exist, Rocket Chip is configured to use published artifacts for Chisel 3 and FIRRTL.

### How to bump

Due to supporting both published artifacts and building from source, we must bump both approaches.
To bump the source dependencies, bump the git submodules as described in the previous section: [Bumping Submodules](#bumping).
To bump the published dependencies, bump the versions at the top of the SBT build file: [build.sbt](build.sbt).
Typically, the SBT dependency will only list a version for Chisel 3 which itself depends on FIRRTL.

Imporantly, the git submodule dependency and published dependency should be kept in sync.
Each published version corresponds to a tag in the respective git repository.
For example, Chisel version `"3.4.0"` in `build.sbt` corresponds to tag `v3.4.0` in the `chisel3` git submodule.
Due to how Wit dependencies work, we do *not* want the submodules to point to release tags; rather, we want them to point to the merge-base with the stable release branch.

For Chisel `v3.4.0`, the stable release branch is `3.4.x`.
Thus, in bumping to Chisel `v3.4.0`, we would want to bump the submodule to the commit given by the command:

```bash
git merge-base v3.4.0 origin/3.4.x
```

Note that `origin/` is necessary for `3.4.x` because it is a branch so will not exist in your local clone by default.
