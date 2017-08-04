# WACK

A super-simple package manager for SWI-Prolog.

To keep things super-simple, WACK makes the following assumptions:

  1. Your package is hosted in a public Github repository.

  2. Your package has a JSON file `WACK.json' in its root that
     documents the dependencies of your package.  See
     https:://github.com/wouterbeek/plRdf/blob/master/WACK.json for an
     example.

  3. Your package uses Git annotated tags (created with `git tag -a
     vX.Y.Z -m '…'`) that denote the package version with semantic
     versioning (`vX.Y.Z`).
