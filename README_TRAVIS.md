# Travis Notes for Administrators

Administrators: Are PRs taking > 1 hr to run through Travis? If you look at the Travis logs is it building `rocket-tools`?

This is because someone committed a PR to `master` which bumped `rocket-tools` and the master cache needs to be updated.
This is the procedure to follow to get things fast again. We don't generally branch updates (e.g. to `master`), just PRs.

To get the `master` cache good again:
----------------------------------

1. Wait for the PR that is changing `rocket-tools` to go green.
2. On Travis, click `More Options -> Caches` on the upper right.
3. Click `Delete` for the `master` Cache. 
4. Click `More Options->Settings`
5. On the `General Settings` section, switch the `Build Branch Updates` toggle to `ON`.
6. Perform the PR's merge to `master`. This will cause the `master` cache to build `rocket-tools`.
7. Once the merge commit goes green on Travis, switch the `Build Branch Updates` toggle to `OFF`.

For other PRs which were happening in parallel to the bump of `rocket-tools`:
----------------------------------------------------------------------------

If your PR already has a cache and you want to keep doing development with the old version of `rocket-tools`, no action is needed. 

If you want to merge or rebase your PR on top of `master` with the new version of `rocket-tools`, you should delete your PR branch's local cache. Otherwise it will rebuild the branch cache instead of using `master`'s cache. To do this:

1. Wait for the previous steps to go through so that the `master` cache is done. 
2. On Travis, click `More Options -> Caches` on the upper right.
3. Click `Delete` for your PR branch's cache.
4. Push updates to the PR as usual, it should download the new `master` cache.
