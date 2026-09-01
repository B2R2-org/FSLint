# Changelog

All notable changes to the FSLint extension.

## 1.2.2

- Add indentation checker for strict mode
- Remove record brace rules, remain only symmetric checker
- Fixed a finding in a project or solution file ending the whole run.

## 1.2.1

- Speed up a whole-tree run
- Fixed spacing read wrongly where a gap holds more than one comment, as in
  `[ (* a *) (* b *) 1 ]`. The comments standing in a gap belong to the code
  beside it, and only the nearest of them was taken as such, so what was left
  of the gap got measured from the middle of the run.
- One comment in a gap always read correctly, and two or more did not:
  `Use single whitespace between bracket and element` and
  `Use single whitespace before ':'` were reported on gaps holding exactly one
  space, `Use single whitespace before 'then'` and
  `Use single whitespace after 'then'` the same way under strict mode, and
  `Remove whitespace before ','` pointed into the comment run rather than at
  the space it meant.

## 1.2.0

- Added a row-length rule for strict mode: a binding body may run to 42 rows,
  and past that the binding is asked to be split. The report names the binding
  rather than underlining the whole of its body.
- Exempted a body built around a long `match` or loop: one running past 35 rows
  takes its length from how many cases or steps there are, so splitting the
  body around it leaves it no shorter. A shorter one is a branch taken in
  passing and rescues nothing.
- Exempted a body that is one piece of data written out -- a record, a
  collection, a `seq`, a multiline string, or whatever shapes one on the way --
  and a test, which is one scenario and means nothing cut in three.
- Counted a binding written inside another once, in the body holding it, so
  that the outermost is where the splitting starts.
- Added `Split into smaller functions`.
- Fixed rank-N array annotations being read as whitespace: `int[,]` was
  reported as an `int[ ]`.
- Fixed blank lines inside a multiline list or array being reported as trailing
  separators.
- Fixed a semicolon inside an element's own text -- a line of a multiline
  string, say -- being reported as the literal's separator.
- Fixed a separator left at the end of the line a literal opens on going
  unreported.

## 1.1.9

- Removed the 80-column line length check.

## 1.1.8

- Added a line-break convention for strict mode: a separator list stays on one
  line while the whole of it fits, and once it does not, every gap between its
  members must agree.
- Added `Use consistent line breaks`, `Bind to fit the line`,
  `Move 'when' to the next line`, and `Align 'and' with 'when'`.
- Extended the convention to tuples, argument and parameter lists, `&&`/`||`
  chains, `if`/`elif`/`else` links, bitwise chains, array and list literals,
  type-parameter constraints, and `try`/`with` blocks.
- Reported the whole of what must move as one warning instead of one per break.
- Fixed the missing whitespace check before `)` on a single-line parenthesis.
- Fixed whitespace checks skipping `struct (...)`, in expressions and patterns.
- Fixed false whitespace reports where a comment sits inside the parentheses.
- Fixed `Bind to fit the line` on a fully parenthesised `if` condition.
- Fixed two strict-only checks running without `--strict`.
- Removed the `Use consistent indentation` rule.
- Stopped stray FCS trace output reaching the console.

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
