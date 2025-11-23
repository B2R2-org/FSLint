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
{
  [<JsonProperty("line")>]
  line: int
  [<JsonProperty("character")>]
  character: int
}
[<CLIMutable>]
type Range = {
  [<JsonProperty("start")>]
  start: Position
  [<JsonProperty("end")>]
  ``end``: Position
}
[<CLIMutable>]
type Diagnostic = {
  [<JsonProperty("range")>]
  range: Range
  [<JsonProperty("severity")>]
  severity: int
  [<JsonProperty("source")>]
  source: string
  [<JsonProperty("message")>]
  message: string
}
[<CLIMutable>]
type SaveOptions = {
  [<JsonProperty("includeText")>]
  includeText: bool
}
[<CLIMutable>]
type TextDocumentSyncOptions = {
  [<JsonProperty("openClose")>]
  openClose: bool
  [<JsonProperty("change")>]
  change: int
  [<JsonProperty("save")>]
  save: SaveOptions option
}
[<CLIMutable>]
type ServerCapabilities = {
  [<JsonProperty("positionEncoding")>]
  positionEncoding: string
  [<JsonProperty("textDocumentSync")>]
  textDocumentSync: TextDocumentSyncOptions
}
[<CLIMutable>]
type ServerInfo = {
  [<JsonProperty("name")>]
  name: string
  [<JsonProperty("version")>]
  version: string
}
[<CLIMutable>]
type InitializeResult = {
  [<JsonProperty("capabilities")>]
  capabilities: ServerCapabilities
  [<JsonProperty("serverInfo")>]
  serverInfo: ServerInfo
}
type LspServer(rpc: JsonRpc) =
  /// Diagnostic 생성
  let createDiagnostic (lineNum: int) (message: string) : Diagnostic =
    {
      range = {
        start = { line = lineNum - 1; character = 0 }
        ``end`` = { line = lineNum - 1; character = 1000 }
      }
      severity = 2 // Warning
      source = "FSLint"
      message = message
    }
  /// LintException을 Diagnostic으로 변환
  let lintExceptionToDiagnostic (ex: Utils.LintException) : Diagnostic option =
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
          | true, lineNum -> Some (createDiagnostic lineNum message)
          | _ -> None
        else None
      else None
    with
    | _ -> None
  /// 파일 린팅
  let lintFile (uri: string) (content: string) : Diagnostic[] =
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
/// Diagnostics 발행
  let publishDiagnostics (uri: string) (diagnostics: Diagnostic[]) : Task =
    let notification = dict [
      "uri", box uri
      "diagnostics", box diagnostics
    ]
    rpc.NotifyAsync("textDocument/publishDiagnostics", notification)
  [<JsonRpcMethod("initialize")>]
  member _.Initialize(p: JToken) : Task<InitializeResult> =
    task {
      eprintfn "Initialize called - START"
      let result = {
        capabilities = {
          positionEncoding = "utf-16"
          textDocumentSync = {
            openClose = true
            change = 1
            save = Some { includeText = true }
          }
        }
        serverInfo = {
          name = "FSLint Language Server"
          version = "1.0.0"
        }
      }
      let json = JsonConvert.SerializeObject(result, Formatting.Indented)
      return result
    }
  [<JsonRpcMethod("initialized")>]
  member _.Initialized(p: JToken) : Task =
    task {
      eprintfn "Initialized notification received"
    }
  [<JsonRpcMethod("textDocument/didOpen")>]
  member _.TextDocumentDidOpen(p: JToken) : Task =
    task {
      let doc = p.["textDocument"]
      let uri = doc.["uri"].ToString()
      let content = doc.["text"].ToString()
      let diagnostics = lintFile uri content
      do! publishDiagnostics uri diagnostics
    }
  [<JsonRpcMethod("textDocument/didChange")>]
  member _.TextDocumentDidChange(p: JToken) : Task =
    task {
      let doc = p.["textDocument"]
      let uri = doc.["uri"].ToString()
      let changes = p.["contentChanges"] :?> JArray
      if changes.Count > 0 then
        let lastChange = changes.[changes.Count - 1]
        let content = lastChange.["text"].ToString()
        let diagnostics = lintFile uri content
        do! publishDiagnostics uri diagnostics
    }
  [<JsonRpcMethod("textDocument/didSave")>]
  member _.TextDocumentDidSave(p: JToken) : Task =
    task {
      let doc = p.["textDocument"]
      let uri = doc.["uri"].ToString()
      match p.["text"] with
      | null -> ()
      | text ->
        let content = text.ToString()
        let diagnostics = lintFile uri content
        do! publishDiagnostics uri diagnostics
    }
  [<JsonRpcMethod("textDocument/didClose")>]
  member _.TextDocumentDidClose(p: JToken) : Task =
    task {
      let doc = p.["textDocument"]
      let uri = doc.["uri"].ToString()
      do! publishDiagnostics uri [||]
    }
  [<JsonRpcMethod("workspace/didChangeConfiguration")>]
  member _.WorkspaceDidChangeConfiguration(p: JToken) : Task =
    task {
      ()
    }
  [<JsonRpcMethod("workspace/didChangeWatchedFiles")>]
  member _.WorkspaceDidChangeWatchedFiles(p: JToken) : Task =
    task {
      ()
    }
  [<JsonRpcMethod("shutdown")>]
  member _.Shutdown() : Task<obj> =
    task {
      eprintfn "Shutdown called"
      return null
    }
  [<JsonRpcMethod("exit")>]
  member _.Exit() =
    eprintfn "Exit called"
    Environment.Exit(0)
[<EntryPoint>]
let main argv =
  try
    eprintfn "FSLint Language Server starting..."
    let stdin = Console.OpenStandardInput()
    let stdout = Console.OpenStandardOutput()
    Console.SetOut(Console.Error)
    use rpc = new JsonRpc(stdout, stdin)
    // JSON-RPC 메시지 로깅 - Warning 레벨 이상만 출력
    rpc.TraceSource <- new System.Diagnostics.TraceSource(
      "JsonRpc", System.Diagnostics.SourceLevels.Warning)
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
  | ex ->
    eprintfn "Fatal error: %s\n%s" ex.Message ex.StackTrace
    1