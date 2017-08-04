# WACK

A super-simple package manager for SWI-Prolog.

To keep things super-simple, WACK makes the following assumptions:

  1. Your package is hosted in a public Github repository.

  2. Your package has a JSON file `wack.json' in its root that
     contains the keys `owner' and `name'.

  3. Your package uses Git annotated tags (`git tag -a vX.Y.Z -m '…'`)
     that denote the semantic version (`vX.Y.Z`) of the package.
