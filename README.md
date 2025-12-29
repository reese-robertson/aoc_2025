# Advent of Code 2025

Project tracking my Advent of Code 2025 work. This year's project is working with:
- [OCaml](https://ocaml.org/about) language
- [Jujutsu](https://github.com/jj-vcs/jj) version control system

Note that Jujutsu is a wrapper around git and client-side only, so no side effects are likely to be visible
here.

## Getting started

Puzzle inputs are expected in a directory `puzzle-inputs/` at the repository top level and should be named by
day, e.g. `puzzle-inputs/day1.txt`.

To solve a day
```
dune exec day2
```

To run tests
```
dune runtest
```