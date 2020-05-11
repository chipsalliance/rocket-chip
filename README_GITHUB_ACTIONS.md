# GitHub Actions Notes for Maintainers

rocket-chip makes use of [GitHub Actions](https://github.com/features/actions)
for running continuous integration (CI) tests. For more information on GitHub
Actions, please see their [documentation](https://help.github.com/en/actions).


## How It Is Configured

The main configuration file describing the continuous integration workflow is
located at
[.github/workflows/continuous-integration.yml](.github/workflows/continuous-integration.yml).
This sets up a few jobs for prebuilding and caching a few tools, such as the
riscv-tools and Verilator, as well as a large matrix job that runs the suite of
tests in parallel.

The actual Makefile targets run as part of the test suites defined in
[regression/run-test-bucket](regression/run-test-bucket).

For information on the Workflow file syntax, please see the
[API reference](https://help.github.com/en/actions/reference/workflow-syntax-for-github-actions).


## Caching

Caching is handled by the official GitHub Action [actions/cache](https://github.com/marketplace/actions/cache).
Caches are kept around for one week, and caches are searched in order branch,
base branch (e.g. PR target), and default branch (e.g. master).

See the [general GitHub Actions documentation on caching](https://help.github.com/en/actions/configuring-and-managing-workflows/caching-dependencies-to-speed-up-workflows)
or the [GitHub repository for actions/cache](https://github.com/actions/cache)
for more information.

### Force Invalidating the riscv-tools or Verilator Caches

Normally there is no need to manually invalidate the caches, since they are
automatically invalidated if either [riscv-tools.hash](riscv-tools.hash) or
[verilator.hash](verilator.hash) changes. However, there are situations where
you may need to manually invalidate the caches.

[There is no way to delete a cache](https://github.com/actions/cache/issues/294),
so the recommended approach for invalidating a cache is to modify the cache key.
In the rocket-chip GitHub Action workflow configuration file, the cache keys
each end in `-v#`, where `#` is a number. These numbers can be incremented to
"invalidate" the cache. Each key needs to be updated in all places that it
occurs in the file.

Below are step-by-step instructions for invalidating the riscv-tools cache
(similar instructions will apply to the other caches):

1. Open [.github/workflows/continuous-integration.yml](.github/workflows/continuous-integration.yml)
2. Under the `prepare-riscv-tools-cache` job, locate the step that defines the cache.
   It can be identified by finding the step which contains `uses: actions/cache@*`.
3. Locate the cache key, which is defined under the `key` property.
   Notice that the key ends with something like `-v1`.
   The number in the key may have changed since this documentation was written.
4. Increment the number, e.g. change the key to end in `-v2`.
5. Locate all other usages of that cache key within the file, and similarly
   increment the number at the end to match.
6. Commit these changes and push them to GitHub, which should automatically run
   and detect that you have a new cache key.
