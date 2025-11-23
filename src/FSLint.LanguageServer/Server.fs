module FSLint.LanguageServer.Server

open System
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.Server
open B2R2.FSLint

/// LSP Diagnostic 생성
let createDiagnostic (lineNum: int) (message: string) =
  { Range =
    { Start = { Line = lineNum - 1; Character = 0 }
      End = { Line = lineNum - 1; Character = 1000 } }
    Severity = Some DiagnosticSeverity.Warning
    Code = Some "FSLint"
    Source = Some "FSLint"
    Message = message
    RelatedInformation = None
    Tags = None
    CodeDescription = None
    Data = None }

let lintExceptionToDiagnostic (ex: Utils.LintException) =
  try
    let msg = ex.Message
    let parts = msg.Split([| '\n' |], 2)
    if parts.Length > 0 then
      let firstLine = parts.[0]
      let lineParts = firstLine.Split([| ':' |], 2)
      if lineParts.Length >= 2 then
        let lineStr = lineParts.[0].Replace("Line", "").Trim()
        let message = lineParts.[1].Trim()
        match Int32.TryParse(lineStr) with
        | true, lineNum -> Some(createDiagnostic lineNum message)
        | _ -> None
      else None
    else None
  with
  _ -> None

let lintFile (uri: string) (content: string) =
  try
    Utils.setCurrentFile uri
    Program.linterForFs.Lint(uri, content)
    [||]
  with
  | :? Utils.LintException as ex ->
    match lintExceptionToDiagnostic ex with
    | Some diag -> [| diag |]
    | None -> [||]
  | ex ->
    eprintfn "Error linting %s: %s" uri ex.Message
    [||]

let serverinfo = Some
  { Name = "FSLint Language Server"
    Version = Some "1.0.0" }

let createServer () =
  // Initialize
  let initialize (p: InitializeParams) =
   async {
    eprintfn "Initialize called"
    return { Capabilities = { TextDocumentSync = Some {
          OpenClose = Some true
          Change = Some TextDocumentSyncKind.Full
          WillSave = None
          WillSaveWaitUntil = None
          Save = Some { IncludeText = Some true }
        }
        HoverProvider = None
        CompletionProvider = None
        SignatureHelpProvider = None
        DefinitionProvider = None
        TypeDefinitionProvider = None
        ImplementationProvider = None
        ReferencesProvider = None
        DocumentHighlightProvider = None
        DocumentSymbolProvider = None
        WorkspaceSymbolProvider = None
        CodeActionProvider = None
        CodeLensProvider = None
        DocumentFormattingProvider = None
        DocumentRangeFormattingProvider = None
        DocumentOnTypeFormattingProvider = None
        RenameProvider = None
        DocumentLinkProvider = None
        ColorProvider = None
        FoldingRangeProvider = None
        DeclarationProvider = None
        ExecuteCommandProvider = None
        Workspace = None
        Experimental = None
        PositionEncoding = Some (PositionEncodingKind.Utf16)
      }
      ServerInfo = serverinfo
    }
  }
  // Initialized
  let initialized (_: InitializedParams) = async {
    eprintfn "Initialized"
  }
  // Shutdown
  let shutdown () = async {
    eprintfn "Shutdown"
  }
  // DidChangeConfiguration
  let didChangeConfiguration (_: DidChangeConfigurationParams) = async {
    eprintfn "DidChangeConfiguration"
  }
  // DidOpenTextDocument
  let didOpenTextDocument (client: ILspClient) (p: DidOpenTextDocumentParams) =
   async {
    let uri = p.TextDocument.Uri
    let content = p.TextDocument.Text
    eprintfn "DidOpen: %s" uri
    let diagnostics = lintFile uri content
    do! client.TextDocumentPublishDiagnostics {
      Uri = uri
      Diagnostics = diagnostics
      Version = Some p.TextDocument.Version
    }
  }
  // DidChangeTextDocument
  let didChangeTextDocument (client: ILspClient)
   (p: DidChangeTextDocumentParams) = async {
    let uri = p.TextDocument.Uri
    eprintfn "DidChange: %s" uri
    match p.ContentChanges |> Array.tryLast with
    | Some change ->
      let content = change.Text
      let diagnostics = lintFile uri content
      do! client.TextDocumentPublishDiagnostics {
        Uri = uri
        Diagnostics = diagnostics
        Version = Some p.TextDocument.Version
      }
    | None -> ()
  }
  // DidSaveTextDocument
  let didSaveTextDocument (client: ILspClient)
   (p: DidSaveTextDocumentParams) = async {
    let uri = p.TextDocument.Uri
    eprintfn "DidSave: %s" uri
    match p.Text with
    | Some content ->
      let diagnostics = lintFile uri content
      do! client.TextDocumentPublishDiagnostics {
        Uri = uri
        Diagnostics = diagnostics
        Version = None
      }
    | None -> ()
  }
  // DidCloseTextDocument
  let didCloseTextDocument (client: ILspClient)
   (p: DidCloseTextDocumentParams) = async {
    let uri = p.TextDocument.Uri
    eprintfn "DidClose: %s" uri
    do! client.TextDocumentPublishDiagnostics {
      Uri = uri
      Diagnostics = [||]
      Version = None
    }
  }
  // 더미 핸들러들
  let willSaveTextDocument (_: WillSaveTextDocumentParams) = async { () }
  let willSaveWaitUntilTextDocument
   (_: WillSaveTextDocumentParams) = async { return None }
  let didChangeWatchedFiles (_: DidChangeWatchedFilesParams) = async { () }
  let didCreateFiles (_: CreateFilesParams) = async { () }
  let didRenameFiles (_: RenameFilesParams) = async { () }
  let didDeleteFiles (_: DeleteFilesParams) = async { () }
  // 서버 핸들러 반환
  {
    Initialize = initialize
    Initialized = initialized
    Shutdown = shutdown
    DidChangeConfiguration = didChangeConfiguration
    DidOpenTextDocument = didOpenTextDocument
    DidChangeTextDocument = didChangeTextDocument
    WillSaveTextDocument = willSaveTextDocument
    WillSaveWaitUntilTextDocument = willSaveWaitUntilTextDocument
    DidSaveTextDocument = didSaveTextDocument
    DidCloseTextDocument = didCloseTextDocument
    DidChangeWatchedFiles = didChangeWatchedFiles
    DidCreateFiles = didCreateFiles
    DidRenameFiles = didRenameFiles
    DidDeleteFiles = didDeleteFiles
  }