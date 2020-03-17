Contributing to Rocket Chip
=====================

Thank you for your interest in contributing to Rocket Chip!

## Table of Contents

+ [Bumping Submodules](#bumping)

### <a name="bumping"></a> Bumping Submodules

Several projects are managed as git submodules as well as [Wit](https://github.com/sifive/wit) dependencies.

### When to bump

Most projects will be bumped by developers as needed; however,
sometimes users may wish to speed up the bumping process.
For more stable projects like Chisel 3 and FIRRTL,
please only bump to stable branches as defined by the specific subproject.
As of March 2020 these branches are `3.2.x` in Chisel 3 and `1.2.x` in FIRRTL.

### How to bump

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
git commit -m "<meaningful message about bumping>"
```

If you are bumping `Chisel 3` or `FIRRTL`, it is ideal to include some notes about
major feature or performance improvements in your commit message.

4. Open a Pull Request on Github

Please see the Github documentation for [Pull Requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests)
