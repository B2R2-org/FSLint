module FSLint.LanguageServer.Program

open System
open System.Collections.Generic
open StreamJsonRpc
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open B2R2.FSLint
open System.Threading.Tasks

[<CLIMutable>]
type Position =
  { [<JsonProperty("line")>] Line: int
    [<JsonProperty("character")>] Character: int }

[<CLIMutable>]
type Range =
  { [<JsonProperty("start")>] Start: Position
    [<JsonProperty("end")>] End: Position }

[<CLIMutable>]
type Diagnostic =
  { [<JsonProperty("range")>] Range: Range
    [<JsonProperty("severity")>] Severity: int
    [<JsonProperty("source")>] Source: string
    [<JsonProperty("message")>] Message: string }

[<CLIMutable>]
type SaveOptions =
  { [<JsonProperty("includeText")>] IncludeText: bool }

[<CLIMutable>]
type TextDocumentSyncOptions =
  { [<JsonProperty("openClose")>] OpenClose: bool
    [<JsonProperty("change")>] Change: int
    [<JsonProperty("save")>] Save: SaveOptions option }

[<CLIMutable>]
type ServerCapabilities =
  { [<JsonProperty("positionEncoding")>] PositionEncoding: string
    [<JsonProperty("textDocumentSync")>]
    TextDocumentSync: TextDocumentSyncOptions }

[<CLIMutable>]
type ServerInfo =
  { [<JsonProperty("name")>] Name: string
    [<JsonProperty("version")>] Version: string }

[<CLIMutable>]
type InitializeResult =
  { [<JsonProperty("capabilities")>] Capabilities: ServerCapabilities
    [<JsonProperty("serverInfo")>] ServerInfo: ServerInfo }

type LspServer(rpc: JsonRpc) =

  let createDiagnostic (lineNum: int) (message: string) =
    { Range = { Start = { Line = lineNum - 1; Character = 0 }
                End = { Line = lineNum - 1; Character = 1000 } }
      Severity = 2
      Source = "FSLint"
      Message = message }

  let lintExceptionToDiagnostic (ex: Utils.LintException) =
    try
      let parts = ex.Message.Split([| '\n' |], 2)
      if parts.Length > 0 then
        let lineParts = parts[0].Split([| ':' |], 2)
        if lineParts.Length >= 2 then
          let lineStr = lineParts[0].Replace("Line", "").Trim()
          let message = lineParts[1].Trim()
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

  let publishDiagnostics (uri: string) (diagnostics: Diagnostic[]) =
    rpc.NotifyWithParameterObjectAsync(
      "textDocument/publishDiagnostics",
      JObject(
        JProperty("uri", uri),
        JProperty("diagnostics", JArray(diagnostics))))

  [<JsonRpcMethod("initialize")>]
  member _.Initialize(_: JToken) =
    task {
      let result =
        { Capabilities =
            { PositionEncoding = "utf-16"
              TextDocumentSync =
                { OpenClose = true
                  Change = 1
                  Save = Some { IncludeText = true } } }
          ServerInfo =
            { Name = "FSLint Language Server"; Version = "1.0.0" } }
      return result
    }

  [<JsonRpcMethod("initialized")>]
  member _.Initialized(_: JToken) =
    task {
      ()
    }

  [<JsonRpcMethod("textDocument/didOpen")>]
  member _.TextDocumentDidOpen(p: JToken) =
    task {
      let doc = p["textDocument"]
      let uri = doc["uri"].ToString()
      let content = doc["text"].ToString()
      let diagnostics = lintFile uri content
      do! publishDiagnostics uri diagnostics
    }

  [<JsonRpcMethod("textDocument/didChange")>]
  member _.TextDocumentDidChange(p: JToken) =
    task {
      let doc = p["textDocument"]
      let uri = doc["uri"].ToString()
      let changes = p["contentChanges"] :?> JArray
      if changes.Count > 0 then
        let lastChange = changes[changes.Count - 1]
        let content = lastChange["text"].ToString()
        let diagnostics = lintFile uri content
        do! publishDiagnostics uri diagnostics
    }

  [<JsonRpcMethod("textDocument/didSave")>]
  member _.TextDocumentDidSave(p: JToken) =
    task {
      let doc = p["textDocument"]
      let uri = doc["uri"].ToString()
      match p["text"] with
      | null -> ()
      | text ->
        let content = text.ToString()
        let diagnostics = lintFile uri content
        do! publishDiagnostics uri diagnostics
    }

  [<JsonRpcMethod("textDocument/didClose")>]
  member _.TextDocumentDidClose(p: JToken) =
    task {
      let doc = p["textDocument"]
      let uri = doc["uri"].ToString()
      do! publishDiagnostics uri [||]
    }

  [<JsonRpcMethod("workspace/didChangeConfiguration")>]
  member _.WorkspaceDidChangeConfiguration(_: JToken) =
    task {
      ()
    }

  [<JsonRpcMethod("workspace/didChangeWatchedFiles")>]
  member _.WorkspaceDidChangeWatchedFiles(_: JToken) =
    task {
      ()
    }

  [<JsonRpcMethod("shutdown")>]
  member _.Shutdown() =
    task {
      eprintfn "Shutdown called"
      return null
    }

  [<JsonRpcMethod("exit")>]
  member _.Exit() =
    eprintfn "Exit called"
    Environment.Exit(0)

[<EntryPoint>]
let main _ =
  try
    eprintfn "FSLint Language Server starting..."
    let stdin = Console.OpenStandardInput()
    let stdout = Console.OpenStandardOutput()
    Console.SetOut(Console.Error)
    use rpc = new JsonRpc(stdout, stdin)
    rpc.TraceSource <-
      new System.Diagnostics.TraceSource(
        "JsonRpc",
        System.Diagnostics.SourceLevels.Warning)
    rpc.TraceSource.Listeners.Add(
      new System.Diagnostics.ConsoleTraceListener()) |> ignore
    let server = LspServer(rpc)
    rpc.AddLocalRpcTarget(server)
    rpc.StartListening()
    eprintfn "Server listening..."
    rpc.Completion.Wait()
    eprintfn "FSLint Language Server stopped"
    0
  with
  ex ->
    eprintfn "Fatal error: %s\n%s" ex.Message ex.StackTrace
    1