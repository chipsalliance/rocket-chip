# Travis Notes for Administrators

Administrators: Are PRs taking > 1 hr to run through Travis? If you look at the Travis logs is it building `rocket-tools` or `verilator`?

This is because someone committed a PR to `master` which bumped `riscv-tools` and/or `verilator`
and the master cache needs to be updated.
Note that the master cache expires after 28 days.
This is the procedure to follow to get things fast again.
Note we don't generally build on branch updates (e.g. to `master`), just PRs, which means that building the `master` cache
is a manual process when needed.

To get the `master` cache good again:
----------------------------------

1. Wait for the PR that is changing `rocket-tools` to go green.
2. Make sure you are logged into Travis. If you don't have the options below, try logging out and in again.
3. On Travis, click `More Options -> Caches`.
4. Click `Delete` for the `master` cache. This is necessary because otherwise the cache just accumulates versions of the tools.
5. On Travis, click `More Options -> Trigger Build` on the upper right.
6. Trigger a `master` build with a commit message like "manually triggering master build to rebuild cache".

For other PRs which were happening in parallel to the bump of `rocket-tools`/`verilator`:
----------------------------------------------------------------------------

If your PR already has a cache and you want to keep doing development with the old version of `rocket-tools`/`verilator`, no action is needed. 

If you want to merge or rebase your PR on top of `master` with the new version of `rocket-tools`, you should delete your PR branch's local cache. Otherwise it will rebuild the branch cache instead of using `master`'s cache. To do this:

1. Wait for the previous steps to go through so that the `master` cache is done. 
2. On Travis, click `More Options -> Caches` on the upper right.
3. Click `Delete` for your PR branch's cache.
4. Push updates to the PR as usual, it should download the new `master` cache.
