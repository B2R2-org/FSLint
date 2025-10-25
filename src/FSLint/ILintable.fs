namespace B2R2.FSLint

type ILintable =
  /// Runs the linter on the given text at the specified path.
  abstract Lint:
       path: string
     * txt: string
    -> LintResult