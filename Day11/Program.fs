open System

open Parsers

let rounds =
     IO.File.ReadAllText("input") 
        |> splitInstructionBlocks 
        |> Array.map parseRound 
        |> Array.where (fun x -> x.IsNone) 
        |> Array.map (fun x -> x.Value)
let roundCount = Array.length rounds |> float
let roundSequence = 
    Array.replicate (int (Math.Ceiling((float 20)/roundCount))) rounds |> Array.collect (fun x -> x) |> Array.take 20
//TOOO: deal with queue state

printfn "Hello from F#"
