module B2R2.FSLint.Configuration

open System
open System.IO

(* TODO: Add Indentation check *)
type EditorConfig = { MaxLineLength: int }

let defaultSettings = { MaxLineLength = 80 }

let private parseSettings lines (fileName: string) =
  let processLine (inSection, settings) (line: string) =
    let trimmed = line.Trim()
    if trimmed = "" || trimmed.StartsWith("#") || trimmed.StartsWith(";") then
      inSection, settings
    elif trimmed.StartsWith("[") && trimmed.EndsWith("]") then
      let section = trimmed.Trim('[', ']')
      let matches =
        section = "*" ||
        section.Contains "*.fs" && fileName.EndsWith ".fs" ||
        (section.Contains "{fs,fsi,fsx}" &&
        (fileName.EndsWith ".fs"))
      matches, settings
    elif inSection && trimmed.Contains "=" then
      let parts = trimmed.Split([| '=' |], 2)
      if parts.Length = 2 then
        let key = parts[0].Trim()
        let value = parts[1].Trim()
        let newSettings =
          match key with
          | "max_line_length" ->
            match Int32.TryParse value with
            | true, length when length > 0 ->
              { settings with MaxLineLength = length }
            | _ ->
              settings
          | _ ->
            settings
        inSection, newSettings
      else
        inSection, settings
    else
      inSection, settings
  lines
  |> Array.fold processLine (false, defaultSettings)
  |> snd

let private readFile (path: string) (maxRetries: int) =
  let rec attempt retryCount =
    try
      use stream = new FileStream(
        path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite
      )
      use reader = new StreamReader(stream)
      let rec readLines acc =
        let line = reader.ReadLine()
        if isNull line then List.rev acc |> Array.ofList
        else readLines (line :: acc)
      readLines []
    with
    | :? IOException when retryCount < maxRetries ->
      Threading.Thread.Sleep 100
      attempt (retryCount + 1)
    | _ ->
      reraise ()
  attempt 0

let private findEditorConfig (startPath: string) =
  let rec search (dir: string) =
    if String.IsNullOrEmpty dir then
      None
    else
      let configPath = Path.Combine(dir, ".editorconfig")
      if File.Exists configPath then
        Some configPath
      else
        let parent = Directory.GetParent dir
        if isNull parent then None else search parent.FullName
  search startPath

let getSettings (rootPath: string): EditorConfig =
  try
    let dir =
      if File.Exists rootPath then Path.GetDirectoryName(rootPath)
      elif Directory.Exists rootPath then rootPath
      else rootPath
    match findEditorConfig dir with
    | Some configPath ->
      let lines = readFile configPath 5
      let settings = parseSettings lines "*.fs"
      settings
    | None ->
      defaultSettings
  with _ ->
    defaultSettings