module B2R2.FSLint.LanguageServer.Program

open System
open System.IO
open System.Threading.Tasks
open StreamJsonRpc
open Newtonsoft.Json.Linq
open FSharp.Compiler.Text
open B2R2.FSLint
open B2R2.FSLint.Diagnostics

type LspServer(rpc: JsonRpc) =
  let mutable workspaceRoot: string option = None
  let mutable hasScannedWorkspace = false
  let debounceTimers =
    Collections.Concurrent.ConcurrentDictionary<string, Threading.Timer>()

  let toLspRange (range: range): LspRange =
    try
      let startLine = max 0 (range.StartLine - 1)
      let startChar = max 0 range.StartColumn
      let endLine = max 0 (range.EndLine - 1)
      let endChar = max 1 range.EndColumn
      let finalStartLine, finalStartChar, finalEndLine, finalEndChar =
        if startLine > endLine || (startLine = endLine && startChar > endChar)
        then endLine, endChar, startLine, startChar
        else startLine, startChar, endLine, endChar
      { Start = { Line = finalStartLine; Character = finalStartChar }
        End = { Line = finalEndLine; Character = finalEndChar } }
    with ex ->
      eprintfn "[RANGE] ERROR converting range: %s" ex.Message
      { Start = { Line = 0; Character = 0 }
        End = { Line = 0; Character = 1 } }

  let toLspDiagnostic (error: LintError): LspDiagnostic option =
    try
      let range = toLspRange error.Range
      if range.Start.Line < 0 || range.End.Line < 0 then
        eprintfn "[DIAG] Invalid range for: %s" error.Message
        None
      else
        Some { Range = range
               Severity = 2
               Source = "FSLint"
               Message = error.Message }
    with ex ->
      eprintfn "[DIAG] ERROR creating diagnostic: %s - %s"
        error.Message ex.Message
      None

  let lintDocument (uri: string) (content: string): LspDiagnostic[] =
    try
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
      with :? LintException as ex ->
        eprintfn "[LINT] LintException: %s" ex.Message
      setCurrentLintContext None
      let diagnostics =
        context.Errors
        |> List.rev
        |> List.choose toLspDiagnostic
        |> Array.ofList
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
      let validDiagnostics =
        diagnostics
        |> Array.filter (fun diag ->
          diag.Range.Start.Line >= 0 &&
          diag.Range.Start.Character >= 0 &&
          diag.Range.End.Line >= 0 &&
          diag.Range.End.Character >= 0 &&
          (diag.Range.Start.Line < diag.Range.End.Line
          || (diag.Range.Start.Line = diag.Range.End.Line
          && diag.Range.Start.Character <= diag.Range.End.Character))
        )
      (* First catch *)
      |> Array.groupBy (fun diag -> diag.Range.Start.Line)
      |> Array.map (fun (line, diags) ->
        diags |> Array.minBy (fun d -> d.Range.Start.Character)
      )
      if validDiagnostics.Length < diagnostics.Length then
        eprintfn "[LSP] WARNING: Filtered %d invalid diagnostics"
          (diagnostics.Length - validDiagnostics.Length)
      else
        ()
      let diagnosticsArray = JArray()
      for diag in validDiagnostics do
        let diagObj =
          JObject(
            JProperty("range",
              JObject(
                JProperty("start",
                  JObject(
                    JProperty("line", diag.Range.Start.Line),
                    JProperty("character", diag.Range.Start.Character))),
                JProperty("end",
                  JObject(
                    JProperty("line", diag.Range.End.Line),
                    JProperty("character", diag.Range.End.Character))))),
            JProperty("severity", diag.Severity),
            JProperty("source", diag.Source),
            JProperty("message", diag.Message))
        diagnosticsArray.Add(diagObj)
      rpc.NotifyWithParameterObjectAsync(
        "textDocument/publishDiagnostics",
        JObject(
          JProperty("uri", uri),
          JProperty("diagnostics", diagnosticsArray)))
    with ex ->
      eprintfn "[LSP] ERROR publishing diagnostics: %s" ex.Message
      eprintfn "[LSP] STACK: %s" ex.StackTrace
      Task.FromResult(())

  let scanWorkspace () =
    async {
      match workspaceRoot with
      | None ->
        eprintfn "[SCAN] ERROR: No workspace root set"
      | Some root ->
        try
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
              |> Seq.toArray
            let batchSize = Environment.ProcessorCount
            let batches =
              fsFiles
              |> Array.chunkBySize batchSize
            for batch in batches do
              let! results =
                batch
                |> Array.map (fun file ->
                  async {
                    try
                      let diagnostics = lintFile file
                      let normalizedPath = file.Replace("\\", "/")
                      let uri =
                        if normalizedPath.[0] = '/' then
                          sprintf "file://%s" normalizedPath
                        else
                          sprintf "file:///%s" normalizedPath
                      do! publishDiagnostics uri diagnostics |> Async.AwaitTask
                      return Ok file
                    with ex ->
                      eprintfn "[SCAN] ERROR processing %s: %s" file ex.Message
                      return Error(file, ex.Message)
                  })
                |> Async.Parallel
              do! Async.Sleep 10
            eprintfn "[SCAN] Completed workspace scan"
        with ex ->
          eprintfn "[SCAN] FATAL ERROR: %s" ex.Message
          eprintfn "[SCAN] STACK: %s" ex.StackTrace
    }

  [<JsonRpcMethod("initialize")>]
  member _.Initialize(p: JToken) =
    let rootUri = p["rootUri"]
    if not (isNull rootUri) then
      let uriStr = rootUri.ToString()
      try
        let decodedUri = Uri.UnescapeDataString(uriStr)
        let uri = Uri(decodedUri)
        let localPath = uri.LocalPath
        workspaceRoot <- Some localPath
      with ex ->
        eprintfn "[LSP] ERROR parsing URI: %s - %s" uriStr ex.Message
        let path =
          if uriStr.StartsWith("file:///") then
            let rawPath = uriStr.Substring(8)
            let decoded = Uri.UnescapeDataString(rawPath)
            if decoded.Length >= 3 && decoded.[0] = '/' && decoded.[2] = ':'
            then decoded.Substring(1).Replace("/", "\\")
            else decoded.Replace("/", "\\")
          elif uriStr.StartsWith("file://") then
            let rawPath = uriStr.Substring(7)
            Uri.UnescapeDataString(rawPath).Replace("/", "\\")
          else
            Uri.UnescapeDataString(uriStr)
        eprintfn "[LSP] Workspace root (fallback): %s" path
        workspaceRoot <- Some path
    else
      eprintfn "[LSP] WARNING: No rootUri"
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
    task {
      match workspaceRoot with
      | None ->
        eprintfn "[LSP] WARNING: Cannot scan - no workspace root"
      | Some _ ->
        eprintfn "[LSP] Triggering workspace scan..."
        hasScannedWorkspace <- true
        scanWorkspace () |> Async.Start
      ()
    }

  [<JsonRpcMethod("textDocument/didOpen")>]
  member _.DidOpen(p: JToken) =
    async {
      try
        if not hasScannedWorkspace && workspaceRoot.IsSome then
          eprintfn "[LSP] WARNING: Initialized didn't scan, scanning now"
          hasScannedWorkspace <- true
          do! scanWorkspace ()
        else
          ()
      with ex ->
        eprintfn "[LSP] ERROR in didOpen: %s" ex.Message
    } |> Async.StartAsTask :> Task

  [<JsonRpcMethod("textDocument/didChange")>]
  member _.DidChange(p: JToken) =
    async {
      try
        let uri = p["textDocument"].["uri"].ToString()
        let changes = p["contentChanges"] :?> JArray
        if changes.Count > 0 then
          let text = changes.[changes.Count - 1].["text"].ToString()
          match debounceTimers.TryGetValue(uri) with
          | true, timer -> timer.Dispose()
          | _ -> ()
          let timer = new Threading.Timer(
            (fun _ ->
              async {
                let diagnostics = lintDocument uri text
                do! publishDiagnostics uri diagnostics |> Async.AwaitTask
              } |> Async.Start
            ),
            null, 300, Threading.Timeout.Infinite)
          debounceTimers.[uri] <- timer
        else
          ()
      with ex ->
        eprintfn "[LSP] ERROR in didChange: %s" ex.Message
    } |> Async.StartAsTask :> Task

  [<JsonRpcMethod("textDocument/didSave")>]
  member _.DidSave(p: JToken) =
    async {
      try
        let uri = p["textDocument"].["uri"].ToString()
        match p["text"] with
        | null -> ()
        | text ->
          let content = text.ToString()
          let diagnostics = lintDocument uri content
          do! publishDiagnostics uri diagnostics |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didSave: %s" ex.Message
    } |> Async.StartAsTask :> Task

  [<JsonRpcMethod("textDocument/didClose")>]
  member _.DidClose(p: JToken) = Task.CompletedTask

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
      new Diagnostics.TraceSource(
        "FSLintLSP", Diagnostics.SourceLevels.Information)
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