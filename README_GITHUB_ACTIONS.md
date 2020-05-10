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

[There is no way to delete a cache](https://github.com/actions/cache/issues/294),
so the recommended approach for invalidating a cache is to modify the cache key.
In the rocket-chip GitHub Action workflow configuration file, the cache keys
each end in a number. These numbers can be incremented to "invalidate" the
cache. Each key needs to be updated in all places that it occurs in the file.

See the [general GitHub Actions documentation on caching](https://help.github.com/en/actions/configuring-and-managing-workflows/caching-dependencies-to-speed-up-workflows)
or the [GitHub repository for actions/cache](https://github.com/actions/cache)
for more information.
