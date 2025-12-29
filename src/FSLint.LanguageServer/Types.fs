namespace FSLint.LanguageServer

type LspPosition =
  { Line: int
    Character: int }

type LspRange =
  { Start: LspPosition
    End: LspPosition }

type LspDiagnostic =
  { Range: LspRange
    Severity: int
    Source: string
    Message: string }

