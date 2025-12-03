module B2R2.FSLint.LineConvention

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Text

let private trailingWhiteSpace = Regex @"\s$"

let [<Literal>] private WindowsLineEnding = "\r\n"

let checkWindowsLineEndings (txt: string) =
  if txt.Contains WindowsLineEnding then
    raiseWithError "Windows line endings are not allowed. Use LF instead."
  else
    ()

let checkControlChar (txt: string) =
  if txt |> String.exists Char.IsControl then
    txt
    |> String.filter Char.IsControl
    |> String.iter (fun c ->
      Console.WriteLine $"Control char: {c} (0x{int c:X2})")
    raiseWithError "File contains control characters. Please remove them."
  else
    ()

let check (txt: string) =
  checkWindowsLineEndings txt
  match Utils.getCurrentLintContext () with
  | Some context ->
    let src = context.Source
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      let lineNum = i + 1
      if line.Length > Utils.MaxLineLength then
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum Utils.MaxLineLength)
            (Position.mkPos lineNum line.Length)
        reportError src range
          $"Line {lineNum} exceeds {Utils.MaxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        let trailingStart = line.TrimEnd().Length
        let range =
          Range.mkRange ""
            (Position.mkPos lineNum trailingStart)
            (Position.mkPos lineNum line.Length)
        reportError src range
          $"Line {lineNum} contains trailing whitespace."
      else
        checkControlChar line
    )
  | None ->
    txt.Split([| "\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      if line.Length > Utils.MaxLineLength then
        Console.WriteLine line
        Console.WriteLine(
          "|" + String.replicate (Utils.MaxLineLength - 2) "-" + "|")
        raiseWithError $"Line {i + 1} exceeds {Utils.MaxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        Console.WriteLine line
        Console.WriteLine(String.replicate (line.Length - 1) " " + "^")
        raiseWithError $"Line {i + 1} contains trailing whitespace."
      else
        checkControlChar line
    )