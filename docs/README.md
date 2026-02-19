# Generated Documentation

This directory contains documentation on the code within this repository.
Documents can either be written directly in markdown, or use embedded `mdoc`,
which compiles against the Rocket Chip (and dependencies) codebase as part of
the PR CI checks, forcing the documentation to remain current with the codebase.

The `src` folder contains the source from which these are generated. To generate
the documentation, run `docs/mdoc` from SBT, making sure that SBT is running
from this repository's root directory with sufficient memory.
To be precise:

```
cd rocket-chip
sbt -mem 4096
```
```
sbt::rocketchip> docs/mdoc
```

The generated documents will appear in
the `generated` folder.