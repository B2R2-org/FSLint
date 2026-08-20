# Changelog

All notable changes to the FSLint extension.

## 1.1.7

- Disable FSLintLSP diagnostic logging

## 1.1.6

- Added support for omitted TypeApp expressions.

## 1.1.5

- Fixed crashes on various expressions
- Fixed duplicate warnings
- Fixed a misleading line-ending message in the language server.

## 1.1.4

- Fixed colon-spacing checks for dotted typeAnnotations.

## 1.1.3

- Fixed a crash on type annotations with empty type arguments.
- Stopped emitting spurious warnings on `extern` declarations.
- Improved the self-identifier warning to suggest `_` or `this` based on usage.

## 1.1.2

- Fixed deconstructive bindings.
- Upgraded package handling to suppress unnecessary warnings.
- Fixed whitespace checking inside function bodies.
- Added the --verbose option.
- Optimized per-file warning reporting for improved diagnostics.

## 1.1.1

- Update package dependencies

## 1.1.0

- Add Anonymous record pattern

## 1.0.7 - 1.0.9

- Add line break check to FunctionCall
- Remove warnings from multiline
- Add missing cases of AnonRecd

## 1.0.6

- Reduce false positives related to spacing rules across various contexts
- Improve detection of missing or incorrect spacing

## 1.0.5

- Add Strict Mode

## 1.0.4

- Relax Tuple Convention rules to better handle cases with inline and trailing comments
- Refactor duplicated functions to improve maintainability and reduce redundancy

## 1.0.3

- Relax Declaration and ArrayOrList rules
- Use ParsedInput instead of source-aware algorithm

## 1.0.2

- Linting is no longer performed in real time and is now executed on file save.
