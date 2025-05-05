B2R2.FSLint
===

Linting tool for B2R2 projects written in F#.

## Motivation

We need a lightweight linter for our F# projects. The goal is to have a tool
that can be used in the CI pipeline to ensure that the code adheres to our style
guidelines. Unfortunately, existing tools like
[FSharpLint](https://github.com/fsprojects/FSharpLint) does not meet our needs
and is not actively maintained. So we decided to create our own linter.