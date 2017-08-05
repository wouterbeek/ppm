# Prolog-Package-Manager (PPM)

A very simple package manager for SWI-Prolog.

PPMs are Prolog Packs for which the following assumptions hold:

  1. It is hosted as a public Github repository.

  2. It uses Git annotated tags (created with `git tag -a vX.Y.Z -m
     '…'`) that denote the package version with a semantic version
     string (`vX.Y.Z`).

Prolog Packs that do not follow these assumptions are not PPMs.  They
should be handled with the more versatile `library(prolog_pack)`.
