namespace B2R2.FSLint

module LineConvention =
  open System
  open System.Text.RegularExpressions

  let private trailingWhiteSpace = Regex @"\s$"

  let [<Literal>] private MaxLineLength = 80

  let check (txt: string) =
    txt.Split ([| "\n"; "\r\n" |], StringSplitOptions.None)
    |> Array.iteri (fun i line ->
      if line.Length > MaxLineLength then
        Console.WriteLine line
        Console.WriteLine ("|" + String.replicate (MaxLineLength - 2) "-" + "|")
        raiseWithError $"Line {i + 1} exceeds {MaxLineLength} characters."
      elif trailingWhiteSpace.IsMatch line then
        Console.WriteLine line
        Console.WriteLine (String.replicate (line.Length - 1) " " + "^")
        raiseWithError $"Line {i + 1} contains trailing whitespace."
      else ()
    )