module B2R2.FSLint.LineConvention

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open Diagnostics

let private trailingWhiteSpace = Regex @"\s$"

let [<Literal>] private WindowsLineEnding = "\r\n"

let checkWindowsLineEndings (txt: string) =
  if txt.Contains WindowsLineEnding then
    raiseWithWarn "Use Windows line endings 'LF'"
  else
    ()

let checkControlChar (txt: string) =
  if txt |> String.exists Char.IsControl then
    txt
    |> String.filter Char.IsControl
    |> String.iter (fun c ->
      Console.WriteLine $"Control char: {c} (0x{int c:X2})")
    raiseWithWarn "Remove file contains control characters"
  else
    ()

let check (txt: string) =
  checkWindowsLineEndings txt
  match currentLintContext.Value with
  | Some context ->
    let src = context.Source
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      let lineNum = i + 1
      if line.Length > MaxLineLength then
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum (MaxLineLength - 1))
            (Position.mkPos lineNum line.Length)
        reportWarn src range
          $"exceeds {MaxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        let trailingStart = line.TrimEnd().Length
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum trailingStart)
            (Position.mkPos lineNum line.Length)
        reportWarn src range "Remove trailing whitespace"
      else
        checkControlChar line
    )
  | None ->
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      if line.Length > MaxLineLength then
        Console.WriteLine line
        Console.WriteLine(
          "|" + String.replicate (MaxLineLength - 2) "-" + "|")
        raiseWithWarn $"Line {i + 1} exceeds {MaxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        Console.WriteLine line
        Console.WriteLine(String.replicate (line.Length - 1) " " + "^")
        raiseWithWarn $"Remove trailing whitespace in Line {i + 1}"
      else
        checkControlChar line
    )