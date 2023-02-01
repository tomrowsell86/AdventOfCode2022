open System

open Parsers

let input = IO.File.ReadLines("input.txt") |> splitInstructionBlocks |> List.map (fun x -> Array.ofList x)
let turnAndStartingItems = (List.map parseTurn) input |> List.where (fun x -> not x.IsNone) |> List.map (fun x -> x.Value)
let turns = List.map fst turnAndStartingItems
let queues = List.map (fun (turn,start) -> (turn.Id,start)) turnAndStartingItems |> List.map (fun (x,y) -> (x,(y,0))) |> Map.ofList
let allRounds =  List.replicate 20 turns |> List.collect (fun x -> x) 



let turnFold (state:Map<char,(int[]*int)>) (turn:Turn)=
    let worryLevelMapper round (worryLevel:int) =
         (round.Operation >> round.Test)  worryLevel

    let (queueState, _) = state.[turn.Id]
    Array.map (worryLevelMapper turn) queueState
       |> Array.map (
            fun (divisable,newWorryLevel) -> 
                let destination = 
                    if divisable = true then
                        turn.TrueDestination
                    else
                        turn.FalseDestination
                let (destQueue,destCnt) = state.[destination]
                let (sourceQueue,srcCnt) = state.[destination]
                let result = 
                    Map.add destination ((Array.concat [[|newWorryLevel|]; destQueue]),destCnt) state |>
                    Map.add turn.Id ((Array.except (Array.where (fun x -> x = newWorryLevel) sourceQueue) sourceQueue,srcCnt + 1))
                    
                result
       )
        |> Array.last

let state = List.fold turnFold queues allRounds
//let result = [1 .. 20] |> List.fold turnFold
let (mostActiveId,_) = Map.toList state |> List.maxBy (snd >> snd)  

printfn "Most Active Monkey was : %c" mostActiveId
