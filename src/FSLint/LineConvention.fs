module B2R2.FSLint.LineConvention

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open Diagnostics

let private trailingWhiteSpace = Regex @"\s$"

let [<Literal>] WindowsLineEnding = "\r\n"

let checkWindowsLineEndings (src: ISourceText) (txt: string) =
  if txt.Contains WindowsLineEnding then
    let firstLine = src.GetLineString(0)
    let range =
      Range.mkRange "" (Position.mkPos 1 0) (Position.mkPos 1 firstLine.Length)
    reportWarn src range "Use Unix line endings 'LF'"
    Error()
  else
    Ok()

let checkControlChar (src: ISourceText) (lineNum: int) (line: string) =
  if line |> String.exists Char.IsControl then
    let controlCharIdx = line |> Seq.findIndex Char.IsControl
    let range =
      Range.mkRange ""
        (Position.mkPos lineNum controlCharIdx)
        (Position.mkPos lineNum (controlCharIdx + 1))
    reportWarn src range "Remove file contains control characters"
  else
    ()

/// The rows of the text, as the readers below take them.
let private rowsOf (txt: string) =
  txt.Split([| "\n" |], StringSplitOptions.None)

/// One row, recorded into the context an editor reads.
let private recordRow src maxLineLength lineNum (line: string) =
  if line.Length > maxLineLength then
    Range.mkRange ""
      (Position.mkPos lineNum (maxLineLength - 1))
      (Position.mkPos lineNum line.Length)
    |> fun range -> reportWarn src range $"exceeds {maxLineLength} characters."
  elif trailingWhiteSpace.IsMatch line then
    Range.mkRange ""
      (Position.mkPos lineNum (line.TrimEnd().Length))
      (Position.mkPos lineNum line.Length)
    |> fun range -> reportWarn src range "Remove trailing whitespace"
  else
    checkControlChar src lineNum line

/// One row, written out for a reader at a terminal. Without a context there
/// is nowhere to record a finding, so the first one raises.
let private printRow src maxLineLength lineNum (line: string) =
  if line.Length > maxLineLength then
    Console.WriteLine line
    Console.WriteLine("|" + String.replicate (maxLineLength - 2) "-" + "|")
    raiseWithWarn $"Line {lineNum} exceeds {maxLineLength} characters."
  elif trailingWhiteSpace.IsMatch line then
    Console.WriteLine line
    Console.WriteLine(String.replicate (line.Length - 1) " " + "^")
    raiseWithWarn $"Remove trailing whitespace in Line {lineNum}"
  else
    checkControlChar src lineNum line

/// Every row answers for its width, for what trails it, and for the
/// characters it holds. A file with the wrong line endings is not read row by
/// row at all: what its rows are is the very thing in question.
let check src (txt: string) =
  let hasPassed = checkWindowsLineEndings src txt
  let maxLineLength = getCurrentMaxLineLength ()
  let eachRow reader =
    rowsOf txt |> Array.iteri (fun i line -> reader (i + 1) line)
  match hasPassed, currentLintContext () with
  | Ok(), Some context ->
    eachRow (recordRow context.Source maxLineLength)
  | Ok(), None ->
    eachRow (printRow src maxLineLength)
  | _ ->
    ()
  hasPassed
