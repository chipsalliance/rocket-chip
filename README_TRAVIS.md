# Travis Notes for Administrators

Administrators: Are PRs taking > 1 hr to run through Travis? If you look at the Travis logs is it building `riscv-tools`?

This is because someone committed a PR to `master` which bumped `riscv-tools` and the master cache needs to be updated.
This is the procedure to follow to get 
things fast again. We don't generally build on merges to master, just PRs.

1. Wait for your PR that you want to merge to go green. This will take a long time.
2. On Travis, click `More Options -> Caches` on the upper right.
3. Click `Delete all Repository Caches`. 
4. Click `More Options->Settings`
5. On the `General Settings` section, switch the `Build Branch Updates` toggle to `ON`.
6. perform your PR's merge to master. This will cause the master cache to build `riscv-tools`.
7. Once the merge commit goes green on Travis, switch the `Build Branch Updates` toggle to `OFF`.

