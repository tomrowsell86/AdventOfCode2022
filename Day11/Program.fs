open System

open Parsers

let input = IO.File.ReadAllText("input") |> splitInstructionBlocks 
printfn "Hello from F#"
