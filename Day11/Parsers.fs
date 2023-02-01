module Parsers
open System
let splitEnum =
    StringSplitOptions.TrimEntries

type Turn =
    { Id: char
      Operation: int -> int
      Test: int -> (bool * int)
      TrueDestination: char
      FalseDestination: char }

//let splitInstructionBlocks (input: string) =
  //  input.Split(Environment.NewLine + Environment.NewLine, (splitEnum))

let splitFolder (splitVal:'a) (state:list<list<'a>>*list<'a>) (current:'a)  =
    let (result, currentGroup) = state
    if current = splitVal then  
        (currentGroup::result, [])
    else
        (result, current::currentGroup)

let splitBy input by = 
    let (result,_) = Seq.fold (splitFolder by) ([],[]) input
    result
let splitInstructionBlocks (input: seq<string>) =
    splitBy input ""

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

let parseTurn (block: string[]) =
    match block with
    | [| id; startingItems; operation; test; trueDest; falseDest |] ->
        let operation = (parseOperation operation)

        Some(
            { Id = parseId id
              Operation = operation
              Test = parseTest test
              TrueDestination = parseTestOutcomeDest trueDest
              FalseDestination = parseTestOutcomeDest falseDest }
        , parseStartingItems startingItems )
    | _ -> None

