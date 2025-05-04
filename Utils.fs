[<AutoOpen>]
module B2R2.FSLint.Utils

open System

let exitWithError (message: string) =
  Console.WriteLine message
  exit 1
