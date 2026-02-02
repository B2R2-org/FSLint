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
      Range.mkRange ""
        (Position.mkPos 1 0)
        (Position.mkPos 1 firstLine.Length)
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

let check src (txt: string) =
  let hasPassed = checkWindowsLineEndings src txt
  let maxLineLength = getCurrentMaxLineLength ()
  match hasPassed, currentLintContext.Value with
  | Ok(), Some context ->
    let src = context.Source
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      let lineNum = i + 1
      if line.Length > maxLineLength then
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum (maxLineLength - 1))
            (Position.mkPos lineNum line.Length)
        reportWarn src range
          $"exceeds {maxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        let trailingStart = line.TrimEnd().Length
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum trailingStart)
            (Position.mkPos lineNum line.Length)
        reportWarn src range "Remove trailing whitespace"
      else
        checkControlChar src lineNum line
    )
    hasPassed
  | Ok(), None ->
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      let lineNum = i + 1
      if line.Length > maxLineLength then
        Console.WriteLine line
        Console.WriteLine(
          "|" + String.replicate (maxLineLength - 2) "-" + "|")
        raiseWithWarn $"Line {i + 1} exceeds {maxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        Console.WriteLine line
        Console.WriteLine(String.replicate (line.Length - 1) " " + "^")
        raiseWithWarn $"Remove trailing whitespace in Line {i + 1}"
      else
        checkControlChar src lineNum line
    )
    hasPassed
  | _ ->
    hasPassed