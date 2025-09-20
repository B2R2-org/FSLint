namespace B2R2.FSLint.Tests

open System
open System.IO
open System.Diagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program 


type MockLinter(delayMs:int) =
    interface ILintable with
        member _.Lint(_path, _txt) =
            let sw = Stopwatch.StartNew()
            let mutable acc = 0
            while sw.ElapsedMilliseconds < int64 delayMs do
                acc <- acc + 1
            if acc = Int32.MinValue then () 

[<TestClass>]
type ParallelPerfTests() =

    let createTempFsFiles (n:int) =
        let dir = Path.Combine(Path.GetTempPath(), "FSLintPerf_" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(dir) |> ignore
        for i in 1..n do
            File.WriteAllText(Path.Combine(dir, $"file_{i}.fs"), "let x = 1")
        dir, Directory.GetFiles(dir, "*.fs")

    let cleanup dir =
        try Directory.Delete(dir, true) with _ -> ()

    [<TestMethod>]
    member _.Parallel_is_faster_than_sequential() =
        let nFiles = 32
        let delayPerFileMs = 40
        let dir, files = createTempFsFiles nFiles
        try
            let linter = MockLinter(delayPerFileMs) :> ILintable

            // (1) 직렬 실행
            let swSeq = Stopwatch.StartNew()
            files |> Array.iteri (fun i path -> tryLintToBuffer linter i path |> ignore)
            swSeq.Stop()
            let tSeq = swSeq.ElapsedMilliseconds

            // (2) 병렬 실행 (main 함수에서 쓰는 runParallelPreservingOrder 호출)
            let swPar = Stopwatch.StartNew()
            let hasErrors = runParallelPreservingOrder linter files
            swPar.Stop()
            let tPar = swPar.ElapsedMilliseconds

            
            Console.WriteLine($"Sequential: {tSeq} ms, Parallel: {tPar} ms, hasErrors={hasErrors}")

            // 검증: 병렬이 직렬보다 빨라야 함
            Assert.IsTrue(tPar < tSeq, $"Expected parallel < sequential, but got Seq={tSeq}, Par={tPar}")
        finally
            cleanup dir
