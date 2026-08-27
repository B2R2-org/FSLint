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
  let mutable editorConfig = Configuration.defaultSettings
  let mutable editorConfigWatcher: FileSystemWatcher option = None
  let mutable strictMode = false

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
        error.Message
        ex.Message
      None

  /// The URI a diagnostic is published under. A rooted path already opens
  /// with a separator, so it takes two slashes rather than three.
  let uriOf (path: string) =
    let normalized = path.Replace("\\", "/")
    if normalized[0] = '/' then sprintf "file://%s" normalized
    else sprintf "file:///%s" normalized

  /// A file written with CRLF is reported on its first row. Three places ask
  /// for this and none of them varies it.
  let crlfDiagnostic =
    { Range = { Start = { Line = 0; Character = 0 }
                End = { Line = 0; Character = 1 } }
      Severity = 2
      Source = "FSLint"
      Message = "Use Unix line endings 'LF'" }

  /// `strict` as a settings object carries it, if it carries it at all. The
  /// client sends it under `initializationOptions` when it starts and under
  /// `settings.fslint` when it changes, and both arrive as a token that may
  /// be null at any step.
  let strictOption (settings: JToken) =
    if isNull settings then
      None
    else
      match settings["strict"] with
      | null -> None
      | value -> Some(value.Value<bool>())

  let lintDocument (uri: string) (content: string): LspDiagnostic[] =
    try
      isStrict <- strictMode
      let sourceText = SourceText.ofString content
      let context: LintContext =
        { Errors = []
          Source = sourceText
          FilePath = uri
          EditorConfig = editorConfig }
      setCurrentLintContext (Some context)
      setCurrentFile uri
      try
        match LineConvention.check sourceText content with
        | Ok() ->
          setDirectiveLines (directiveLinesOf sourceText)
          beginReadings ()
          parseFile sourceText uri
          |> List.iter (Program.checkWithAST sourceText)
        | _ ->
          ()
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
        lintDocument (uriOf filePath) content
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
        let startPos =
          JObject(JProperty("line", diag.Range.Start.Line),
                  JProperty("character", diag.Range.Start.Character))
        let endPos =
          JObject(JProperty("line", diag.Range.End.Line),
                  JProperty("character", diag.Range.End.Character))
        let range =
          JObject(JProperty("start", startPos), JProperty("end", endPos))
        let diagObj =
          JObject(JProperty("range", range),
                  JProperty("severity", diag.Severity),
                  JProperty("source", diag.Source),
                  JProperty("message", diag.Message))
        diagnosticsArray.Add(diagObj)
      let payload =
        JObject(JProperty("uri", uri),
                JProperty("diagnostics", diagnosticsArray))
      rpc.NotifyWithParameterObjectAsync("textDocument/publishDiagnostics",
                                         payload)
    with ex ->
      eprintfn "[LSP] ERROR publishing diagnostics: %s" ex.Message
      eprintfn "[LSP] STACK: %s" ex.StackTrace
      Task.FromResult(())

  /// Every F# source under the root, less what is built rather than written.
  let sourceFiles root =
    Directory.EnumerateFiles(root, "*.fs", SearchOption.AllDirectories)
    |> Seq.filter (fun path ->
      not (path.Contains("node_modules") ||
           path.Contains("bin") ||
           path.Contains("obj") ||
           path.Contains(".git")))
    |> Seq.toArray

  /// The project and solution files under the root. `.slnx` is the newer
  /// spelling of a solution and both are looked for.
  let projectFiles root =
    [| yield! Directory.GetFiles(root, "*.fsproj", SearchOption.AllDirectories)
       yield! Directory.GetFiles(root, "*.sln", SearchOption.AllDirectories)
       yield! Directory.GetFiles(root, "*.slnx", SearchOption.AllDirectories) |]

  /// Lints every file, a processor's worth at a time, publishing as it goes.
  /// One file failing must not stop the rest, so each answers for itself.
  let lintInBatches files =
    async {
      for batch in Array.chunkBySize Environment.ProcessorCount files do
        let! _ =
          batch
          |> Array.map (fun file ->
            async {
              try
                let diagnostics = lintFile file
                do! publishDiagnostics (uriOf file) diagnostics
                    |> Async.AwaitTask
                return Ok file
              with ex ->
                eprintfn "[SCAN] ERROR processing %s: %s" file ex.Message
                return Error(file, ex.Message)
            })
          |> Async.Parallel
        do! Async.Sleep 10
    }

  /// A project or solution file written with CRLF is reported on its first
  /// row. These are not F# sources and do not go through the linter.
  let checkLineEndings files =
    async {
      for file in files do
        try
          let content = File.ReadAllText file
          if content.Contains LineConvention.WindowsLineEnding then
            do! publishDiagnostics (uriOf file) [| crlfDiagnostic |]
                |> Async.AwaitTask
          else
            ()
        with ex ->
          eprintfn "[SCAN] ERROR checking CRLF in %s: %s" file ex.Message
    }

  /// True when the URI names a project or solution rather than a source. A
  /// build file is not F# and answers only for its line endings.
  let isProjectFile (uri: string) =
    uri.EndsWith ".fsproj" || uri.EndsWith ".sln" || uri.EndsWith ".slnx"

  /// Publishes what the content's line endings answer for: the one complaint
  /// where they are CRLF, and nothing where they are not. Publishing nothing
  /// is what clears a complaint the last save left behind.
  let publishLineEndings uri (content: string) =
    async {
      if content.Contains LineConvention.WindowsLineEnding then
        do! publishDiagnostics uri [| crlfDiagnostic |] |> Async.AwaitTask
      else
        do! publishDiagnostics uri [||] |> Async.AwaitTask
    }

  let scanWorkspace () =
    async {
      match workspaceRoot with
      | None ->
        eprintfn "[SCAN] ERROR: No workspace root set"
      | Some root ->
        try
          if not (Directory.Exists root) then
            eprintfn "[SCAN] ERROR: Directory does not exist: %s" root
          else
            do! sourceFiles root |> lintInBatches
            do! projectFiles root |> checkLineEndings
            eprintfn "[SCAN] Completed workspace scan"
        with ex ->
          eprintfn "[SCAN] FATAL ERROR: %s" ex.Message
          eprintfn "[SCAN] STACK: %s" ex.StackTrace
    }

  let startEditorConfigWatcher (rootPath: string) =
    try
      let watcher = new FileSystemWatcher()
      watcher.Path <- rootPath
      watcher.Filter <- ".editorconfig"
      watcher.NotifyFilter <- NotifyFilters.LastWrite ||| NotifyFilters.FileName
      watcher.IncludeSubdirectories <- true
      let reloadConfig _ =
        editorConfig <- Configuration.getSettings rootPath
        scanWorkspace () |> Async.Start
      watcher.Changed.Add(reloadConfig)
      watcher.Created.Add(reloadConfig)
      watcher.Deleted.Add(fun _ ->
        eprintfn "[EditorConfig] File deleted, using defaults"
        editorConfig <- Configuration.defaultSettings
      )
      watcher.EnableRaisingEvents <- true
      editorConfigWatcher <- Some watcher
    with ex ->
      eprintfn "[EditorConfig] Failed to start watcher: %s" ex.Message

  let stopEditorConfigWatcher () =
    match editorConfigWatcher with
    | Some watcher ->
      watcher.Dispose()
      editorConfigWatcher <- None
    | None ->
      ()

  /// Takes the workspace as the root the client named.
  let openWorkspace path =
    workspaceRoot <- Some path
    editorConfig <- Configuration.getSettings path
    eprintfn "[LSP] EditorConfig loaded"
    startEditorConfigWatcher path

  /// The local path a `rootUri` names.
  ///
  /// One the framework refuses is read by hand: the scheme comes off, the
  /// escapes are undone, and a Windows drive letter loses the separator that
  /// `file:///C:/...` puts in front of it.
  let pathOfRootUri (uriStr: string) =
    try
      Uri(Uri.UnescapeDataString uriStr).LocalPath
    with ex ->
      eprintfn "[LSP] ERROR parsing URI: %s - %s" uriStr ex.Message
      let path =
        if uriStr.StartsWith "file:///" then
          let decoded = Uri.UnescapeDataString(uriStr.Substring 8)
          if decoded.Length >= 3 && decoded[0] = '/' && decoded[2] = ':'
          then decoded.Substring(1).Replace("/", "\\")
          else decoded.Replace("/", "\\")
        elif uriStr.StartsWith "file://" then
          Uri.UnescapeDataString(uriStr.Substring 7).Replace("/", "\\")
        else
          Uri.UnescapeDataString uriStr
      eprintfn "[LSP] Workspace root (fallback): %s" path
      path

  /// What the server tells the client it can do.
  let capabilities () =
    let sync =
      JObject(JProperty("openClose", true),
              JProperty("change", 0),
              JProperty("save", JObject(JProperty("includeText", true))))
    JObject(JProperty("textDocumentSync", sync),
            JProperty("workspace", JObject(JProperty("configuration", true))))

  /// What the server calls itself.
  let serverInfo () =
    JObject(JProperty("name", "FSLint Language Server"),
            JProperty("version", "1.0.0"))

  [<JsonRpcMethod("initialize")>]
  member _.Initialize(p: JToken) =
    match p["rootUri"] with
    | null ->
      eprintfn "[LSP] WARNING: No rootUri"
      editorConfig <- Configuration.defaultSettings
    | rootUri ->
      openWorkspace (pathOfRootUri (rootUri.ToString()))
    match strictOption p["initializationOptions"] with
    | Some value ->
      strictMode <- value
      isStrict <- value
      eprintfn "[LSP] Strict mode: %b" value
    | None ->
      ()
    JObject(JProperty("capabilities", capabilities ()),
            JProperty("serverInfo", serverInfo ()))

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

  [<JsonRpcMethod("workspace/didChangeConfiguration")>]
  member _.DidChangeConfiguration(p: JToken) =
    task {
      try
        let fslint =
          match p["settings"] with
          | null -> null
          | settings -> settings["fslint"]
        match strictOption fslint with
        | Some value when value <> strictMode ->
          strictMode <- value
          isStrict <- value
          eprintfn "[LSP] Strict mode changed to: %b" value
          scanWorkspace () |> Async.Start
        | _ ->
          ()
      with ex ->
        eprintfn "[LSP] ERROR in didChangeConfiguration: %s" ex.Message
    } :> Task

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

  [<JsonRpcMethod("textDocument/didSave")>]
  member _.DidSave(p: JToken) =
    async {
      try
        let uri = p["textDocument"].["uri"].ToString()
        let text = p["text"]
        if isProjectFile uri then
          match text with
          | null ->
            try
              do! publishLineEndings uri (File.ReadAllText(Uri(uri).LocalPath))
            with ex ->
              eprintfn "[LSP] ERROR reading file in didSave: %s" ex.Message
          | text ->
            do! publishLineEndings uri (text.ToString())
        else
          match text with
          | null ->
            ()
          | text ->
            let diagnostics = lintDocument uri (text.ToString())
            do! publishDiagnostics uri diagnostics |> Async.AwaitTask
      with ex ->
        eprintfn "[LSP] ERROR in didSave: %s" ex.Message
    } |> Async.StartAsTask :> Task

  [<JsonRpcMethod("textDocument/didClose")>]
  member _.DidClose(p: JToken) = Task.CompletedTask

  [<JsonRpcMethod("shutdown")>]
  member _.Shutdown() =
    task { eprintfn "[LSP] Shutdown requested"
           stopEditorConfigWatcher ()
           () }

  [<JsonRpcMethod("exit")>]
  member _.Exit() =
    eprintfn "[LSP] Exit"
    stopEditorConfigWatcher ()
    Environment.Exit(0)

[<EntryPoint>]
let main _ =
  Diagnostics.Trace.Listeners.Clear()
  try
    eprintfn "========================================="
    eprintfn "FSLint Language Server Starting"
    eprintfn "========================================="
    let stdin = Console.OpenStandardInput()
    let stdout = Console.OpenStandardOutput()
    Console.SetOut(Console.Error)
    use rpc = new JsonRpc(stdout, stdin)
    rpc.TraceSource <-
      new Diagnostics.TraceSource("FSLintLSP", Diagnostics.SourceLevels.Off)
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