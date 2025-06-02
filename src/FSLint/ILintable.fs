namespace B2R2.FSLint

type ILintable =
  /// Runs the linter on the given text. The given path is used only for error
  /// reporting.
  abstract Lint: path: string -> txt: string -> unit