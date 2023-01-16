module Parsers
open System
let splitEnum =
    StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries

type Round =
    { Id: char
      StartingQueue: int[]
      Operation: int -> int
      Test: int -> (bool * int)
      TrueDestination: char
      FalseDestination: char }

let splitInstructionBlocks (input: string) =
    input.Split(Environment.NewLine, (splitEnum))

let (|Integer|_|) (input: string) =
    let mutable x = 0
    if Int32.TryParse(input, &x) then Some(x) else None

let parseOperation (input: string) =
    match (input.Split([| ' ' |], splitEnum)) with
    | [| _; op; "old" |] ->
        match op with
        | "*" -> fun y -> y * y
        | "+" -> fun y -> y + y
        | _ -> fun y -> y
    | [| _; op; arg |] ->
        match op with
        | "*" -> fun y -> y * int arg
        | "+" -> fun y -> y + int arg
        | _ -> fun y -> y
    | _ -> fun y -> y


let parseStartingItems (input: string) =
    input.Split([| ':'; ',' |], splitEnum)
    |> Array.map (fun (x: string) ->
        match x with
        | Integer y -> Some(y)
        | _ -> None)
    |> Array.where (fun y -> y <> None)
    |> Array.map (fun x -> x.Value)

let parseId (input: string) = input.ToCharArray()[7]

let parseTest (input: string) =
    match input.Split(' ', splitEnum) with
    | [| _; _; _; divisor |] ->
        let intDivisor = int divisor
        fun x -> (x % intDivisor = 0, x / intDivisor)
    | _ -> fun x -> (false, x)

let parseTestOutcomeDest (outcome: string) = Array.last (outcome.ToCharArray())

let parseRound (block: string[]) =
    match block with
    | [| id; startingItems; operation; test; trueDest; falseDest |] ->
        let startingItems = parseStartingItems startingItems
        let operation = (parseOperation operation)

        Some(
            { Id = parseId id
              StartingQueue = startingItems
              Operation = operation
              Test = parseTest test
              TrueDestination = parseTestOutcomeDest trueDest
              FalseDestination = parseTestOutcomeDest falseDest }
        )
    | _ -> None

