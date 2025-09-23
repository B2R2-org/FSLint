module B2R2.FSLint.LineConvention

open System
open System.Text.RegularExpressions

let private trailingWhiteSpace = Regex @"\s$"

let [<Literal>] private MaxLineLength = 80

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
  txt.Split([| "\n" |], StringSplitOptions.None)
  |> Array.iteri (fun i line ->
    if line.Length > MaxLineLength then
      Console.WriteLine line
      Console.WriteLine("|" + String.replicate (MaxLineLength - 2) "-" + "|")
      raiseWithError $"Line {i + 1} exceeds {MaxLineLength} characters."
    elif trailingWhiteSpace.IsMatch line then
      Console.WriteLine line
      Console.WriteLine(String.replicate (line.Length - 1) " " + "^")
      raiseWithError $"Line {i + 1} contains trailing whitespace."
    elif line.Contains("let") && line.Contains("\"\"\"") then
      Console.WriteLine line
      Console.WriteLine("      ^^^ triple quote with let")
      raiseWithError $"Line {i + 1}: Triple outlet must not be in same line with let statement."
    else
      checkControlChar line
  )