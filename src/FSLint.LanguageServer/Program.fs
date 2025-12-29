module FSLint.LanguageServer.Program

open System
open System.IO
open StreamJsonRpc
open Newtonsoft.Json.Linq
open FSharp.Compiler.Text
open B2R2.FSLint
open B2R2.FSLint.Diagnostics

type LspServer(rpc: JsonRpc) =
  let mutable workspaceRoot: string option = None
  let mutable hasScannedWorkspace = false
  let toLspRange (range: range): LspRange =
    try
      let startLine = max 0 (range.StartLine - 1)
      let startChar = max 0 range.StartColumn
      let endLine = max 0 (range.EndLine - 1)
      let endChar = max 0 range.EndColumn
      let lspRange =
        { Start = { Line = startLine; Character = startChar }
          End = { Line = endLine; Character = endChar } }
      lspRange
    with ex ->
      eprintfn "[RANGE] ERROR converting range: %s" ex.Message
      { Start = { Line = 0; Character = 0 }
        End = { Line = 0; Character = 1 } }

  let toLspDiagnostic (error: LintError): LspDiagnostic option =
    try
      let range = toLspRange error.Range
      if range.start.line < 0 || range.``end``.line < 0 then
        eprintfn "[DIAG] Invalid range for: %s" error.Message
        None
      else
        Some { range = range
               severity = 2
               source = "FSLint"
               message = error.Message }
    with ex ->
      eprintfn "[DIAG] ERROR creating diagnostic: %s - %s"
        error.Message ex.Message
      None

  let lintDocument (uri: string) (content: string): LspDiagnostic[] =
    try
      eprintfn "[LINT] Starting: %s" uri
      let sourceText = SourceText.ofString content
      let context: LintContext =
        { Errors = []
          Source = sourceText
          FilePath = uri }
      setCurrentLintContext (Some context)
      setCurrentFile uri
      try
        LineConvention.check content
        let src, parseTree = Utils.parseFile content uri
        Program.checkWithAST src parseTree
        eprintfn "[LINT] Completed successfully"
      with :? LintException as ex ->
        eprintfn "[LINT] LintException: %s" ex.Message
      setCurrentLintContext None
      let diagnostics =
        context.Errors
        |> List.rev
        |> List.choose toLspDiagnostic
        |> Array.ofList
      eprintfn "[LINT] Created %d valid diagnostics" diagnostics.Length
      diagnostics
    with ex ->
      eprintfn "[LINT] ERROR: %s" ex.Message
      [||]

  let lintFile (filePath: string): LspDiagnostic[] =
    try
      if File.Exists(filePath) then
        let content = File.ReadAllText(filePath)
        let normalizedPath = filePath.Replace("\\", "/")
        let uri =
          if normalizedPath[0] = '/' then sprintf "file://%s" normalizedPath
          else sprintf "file:///%s" normalizedPath
        lintDocument uri content
      else
        eprintfn "[LINT] File not found: %s" filePath
        [||]
    with ex ->
      eprintfn "[LINT] Error reading file %s: %s" filePath ex.Message
      [||]

  let publishDiagnostics (uri: string) (diagnostics: LspDiagnostic[]) =
    try
      eprintfn "[LSP] Publishing %d diagnostics for %s" diagnostics.Length uri
      let validDiagnostics =
        diagnostics
        |> Array.filter (fun diag ->
          diag.range.start.line >= 0 &&
          diag.range.start.character >= 0 &&
          diag.range.``end``.line >= 0 &&
          diag.range.``end``.character >= 0 &&
          diag.range.start.line <= diag.range.``end``.line
        )
      if validDiagnostics.Length < diagnostics.Length then
        eprintfn "[LSP] WARNING: Filtered %d invalid diagnostics"
          (diagnostics.Length - validDiagnostics.Length)
      eprintfn "[LSP] Publishing %d valid diagnostics" validDiagnostics.Length
      let paramsDict = System.Collections.Generic.Dictionary<string, obj>()
      paramsDict.["uri"] <- uri :> obj
      paramsDict.["diagnostics"] <- validDiagnostics :> obj
      eprintfn "[LSP] Sending notification with Dictionary"
      rpc.NotifyAsync("textDocument/publishDiagnostics", paramsDict)
    with ex ->
      eprintfn "[LSP] ERROR publishing diagnostics: %s" ex.Message
      eprintfn "[LSP] STACK: %s" ex.StackTrace
      System.Threading.Tasks.Task.FromResult(())

  let scanWorkspace () =
    async {
      match workspaceRoot with
      | None ->
        eprintfn "[SCAN] ERROR: No workspace root set"
      | Some root ->
        try
          eprintfn "[SCAN] =========================================="
          eprintfn "[SCAN] Starting workspace scan"
          eprintfn "[SCAN] Root: %s" root
          eprintfn "[SCAN] =========================================="
          if not (Directory.Exists(root)) then
            eprintfn "[SCAN] ERROR: Directory does not exist: %s" root
          else
            let fsFiles =
              Directory.EnumerateFiles(root, "*.fs",
                                       SearchOption.AllDirectories)
              |> Seq.filter (fun path ->
                not (path.Contains("node_modules") ||
                      path.Contains("bin") ||
                      path.Contains("obj") ||
                      path.Contains(".git")))
              |> Seq.toList
            eprintfn "[SCAN] Found %d F# files" fsFiles.Length
            for i, file in fsFiles |> List.mapi (fun i f -> i + 1, f) do
              try
                eprintfn "[SCAN] [%d/%d] %s" i fsFiles.Length file
                let diagnostics = lintFile file
                let normalizedPath = file.Replace("\\", "/")
                let uri =
                  if normalizedPath.[0] = '/' then
                    sprintf "file://%s" normalizedPath
                  else
                    sprintf "file:///%s" normalizedPath
                do! publishDiagnostics uri diagnostics |> Async.AwaitTask
                eprintfn "[SCAN] Published %d diagnostics" diagnostics.Length
              with ex ->
                eprintfn "[SCAN] ERROR processing %s: %s" file ex.Message
            eprintfn "[SCAN] =========================================="
            eprintfn "[SCAN] Completed: %d files analyzed" fsFiles.Length
            eprintfn "[SCAN] =========================================="
        with ex ->
          eprintfn "[SCAN] FATAL ERROR: %s" ex.Message
          eprintfn "[SCAN] STACK: %s" ex.StackTrace
    }

  [<JsonRpcMethod("initialize")>]
  member _.Initialize(p: JToken) =
    eprintfn "[LSP] =========================================="
    eprintfn "[LSP] Initialize"
    let rootUri = p.["rootUri"]
    if not (isNull rootUri) then
      let uri = rootUri.ToString()
      eprintfn "[LSP] Root URI: %s" uri
      let path =
        if uri.StartsWith("file:///") then
          let rawPath = uri.Substring(8)
          System.Uri.UnescapeDataString(rawPath).Replace("/", "\\")
        elif uri.StartsWith("file://") then
          let rawPath = uri.Substring(7)
          System.Uri.UnescapeDataString(rawPath).Replace("/", "\\")
        else
          uri.Replace("/", "\\")
      workspaceRoot <- Some path
      eprintfn "[LSP] Workspace root: %s" path
    else
      eprintfn "[LSP] WARNING: No rootUri"
    eprintfn "[LSP] =========================================="
    JObject(
      JProperty("capabilities",
        JObject(
          JProperty("textDocumentSync",
            JObject(
              JProperty("openClose", true),
              JProperty("change", 1),
              JProperty("save", JObject(JProperty("includeText", true)))
            )
          )
        )
      ),
      JProperty("serverInfo",
        JObject(
          JProperty("name", "FSLint Language Server"),
          JProperty("version", "1.0.0")
        )
      )
    )

  [<JsonRpcMethod("initialized")>]
  member _.Initialized(p: JToken) =
    try
      eprintfn "[LSP] =========================================="
      eprintfn "[LSP] Initialized notification received"
      eprintfn "[LSP] Workspace root: %A" workspaceRoot
      eprintfn "[LSP] =========================================="
      match workspaceRoot with
      | None ->
        eprintfn "[LSP] WARNING: Cannot scan - no workspace root"
      | Some _ ->
        eprintfn "[LSP] Triggering workspace scan..."
        hasScannedWorkspace <- true
        scanWorkspace () |> Async.Start
      null
    with ex ->
      eprintfn "[LSP] ERROR in Initialized: %s" ex.Message
      eprintfn "[LSP] STACK: %s" ex.StackTrace
      null

  [<JsonRpcMethod("textDocument/didOpen")>]
  member _.DidOpen(p: JToken) =
    async {
      try
        let uri = p.["textDocument"].["uri"].ToString()
        let text = p.["textDocument"].["text"].ToString()
        eprintfn "[LSP] didOpen: %s" uri
        if not hasScannedWorkspace && workspaceRoot.IsSome then
          eprintfn "[LSP] First didOpen - triggering workspace scan"
          hasScannedWorkspace <- true
          do! scanWorkspace ()
        let diagnostics = lintDocument uri text
        do! publishDiagnostics uri diagnostics |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didOpen: %s" ex.Message
    } |> Async.StartAsTask :> System.Threading.Tasks.Task

  [<JsonRpcMethod("textDocument/didChange")>]
  member _.DidChange(p: JToken) =
    async {
      try
        let uri = p.["textDocument"].["uri"].ToString()
        let changes = p.["contentChanges"] :?> JArray
        if changes.Count > 0 then
          let text = changes.[changes.Count - 1].["text"].ToString()
          eprintfn "[LSP] didChange: %s" uri
          let diagnostics = lintDocument uri text
          do! publishDiagnostics uri diagnostics |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didChange: %s" ex.Message
    } |> Async.StartAsTask :> System.Threading.Tasks.Task

  [<JsonRpcMethod("textDocument/didSave")>]
  member _.DidSave(p: JToken) =
    async {
      try
        let uri = p.["textDocument"].["uri"].ToString()
        match p.["text"] with
        | null -> ()
        | text ->
            let content = text.ToString()
            eprintfn "[LSP] didSave: %s" uri
            let diagnostics = lintDocument uri content
            do! publishDiagnostics uri diagnostics |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didSave: %s" ex.Message
    } |> Async.StartAsTask :> System.Threading.Tasks.Task

  [<JsonRpcMethod("textDocument/didClose")>]
  member _.DidClose(p: JToken) =
    async {
      try
        let uri = p.["textDocument"].["uri"].ToString()
        eprintfn "[LSP] didClose: %s" uri
        do! publishDiagnostics uri [||] |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didClose: %s" ex.Message
    } |> Async.StartAsTask :> System.Threading.Tasks.Task

  [<JsonRpcMethod("textDocument/inlayHint")>]
  member _.InlayHint(p: JToken) =
    System.Threading.Tasks.Task.FromResult(JArray() :> JToken)

  [<JsonRpcMethod("shutdown")>]
  member _.Shutdown() =
    eprintfn "[LSP] Shutdown"
    null

  [<JsonRpcMethod("exit")>]
  member _.Exit() =
    eprintfn "[LSP] Exit"
    Environment.Exit(0)

[<EntryPoint>]
let main _ =
  try
    eprintfn "========================================="
    eprintfn "FSLint Language Server Starting"
    eprintfn "========================================="
    let stdin = Console.OpenStandardInput()
    let stdout = Console.OpenStandardOutput()
    Console.SetOut(Console.Error)
    use rpc = new JsonRpc(stdout, stdin)
    rpc.TraceSource <-
      new System.Diagnostics.TraceSource(
        "FSLintLSP",
        System.Diagnostics.SourceLevels.Information)
    let server = LspServer(rpc)
    rpc.AddLocalRpcTarget(server)
    rpc.StartListening()
    eprintfn "[LSP] Server listening..."
    rpc.Completion.Wait()
    eprintfn "[LSP] Server stopped"
    0
  with ex ->
    eprintfn "FATAL ERROR: %s" ex.Message
    eprintfn "STACK TRACE: %s" ex.StackTrace
    1