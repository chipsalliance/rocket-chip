Contributing to Rocket Chip
=====================

Thank you for your interest in contributing to Rocket Chip!

## Table of Contents

+ [Submitting a PR](#submitting)
+ [Conditions for Merging a PR](#merging)
+ [Bumping Submodules](#bumping)

### <a name="submitting"></a> Submitting a PR

You do not need write access to this repository to make a PR,
you can fork and make a PR from your forked version.

Please see the Github documentation for [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests).

Please ensure to fill out the PR template appropriately and update it if your PR changes.

### <a name="merging"></a> Conditions for Merging a PR

Currently, the requirements for merging a PR are:
 + At least one approved review from an administrator
 + Passing at least one of the GitHub actions checks or the Travis checks (Travis is currently disabled).
 
#### <a name="merging"></a> Automatic Merging with mergify.io

This repository uses https://mergify.io/ to automate some pull request tasks.
Currently, the only rule is that a PR to `master` can be merged automatically if the following conditions are met:
 + At least one approved review from an administrator
 + No disapproving reviews
 + No merge conflicts
 + All GitHub Actions CI checks passing
 + The `DONT MERGE` label is not applied
 + One of the `MERGE WITH MERGIFY` or `SQUASH & MERGE WITH MERGIFY` labels are applied
 
Our Mergify setup enforces a `strict` merge: PRs that match the above conditions will not be merged unless they have passed CI _and are up to date with the current tip of master branch_. Mergify handles updating the matching PRs automatically.
This ensures that two conflicting PRs won't break CI unexpectedly.
However, this means your PR may need to keep running CI if others are merging.
To mitigate the effects of this, we have enabled Mergify's "smart" strict strategy.
Mergify will automatically queue the mergify-managed PRs and update them serially, one at a time.

### <a name="bumping"></a> Bumping Submodules

Several projects are managed as git submodules as well as [Wit](https://github.com/sifive/wit) dependencies.

#### When to bump

Most projects will be bumped by developers as needed; however,
sometimes users may wish to speed up the bumping process.
For more stable projects like Chisel 3 and FIRRTL,
please only bump to stable branches as defined by the specific subproject.
As of March 2020 these branches are `3.2.x` in Chisel 3 and `1.2.x` in FIRRTL.

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
