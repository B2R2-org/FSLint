namespace B2R2.FSLint

type LintResult =
  | Success
  | Failure of errorMessage: string