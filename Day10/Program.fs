let input = System.IO.File.ReadLines("input.txt")

type CycleState =
    { CurrentCycle: int
      ExecutingCmd: string }

let (|NoOp|Add|) (cmd: string) =
    let (parts: string[]) = cmd.Split(' ')

    match parts[0] with
    | "noop" -> NoOp
    | "addx" -> Add(int parts[1])
    | _ -> NoOp

let mapToRecordingCycle (cycle, register) =
    let ranges = [ 20..40..220 ]

    let recordedCycleResult =
        List.tryPick (fun x -> if x = cycle then Some(x) else None) ranges

    if recordedCycleResult = None then
        0
    else
        (recordedCycleResult.Value) * register

let folder (state: List<int * int> * int * int) (line: string) =
    let (registerCycleMap, currentCycle, registerVal) = state

    match (line) with
    | NoOp -> (((currentCycle, registerVal) :: registerCycleMap), currentCycle + 1, registerVal)
    | Add op ->
        ([ (currentCycle + 1, registerVal); ((currentCycle, registerVal)) ]
         @ registerCycleMap,
         currentCycle + 2,
         registerVal + op)

let (result, _, _) = Seq.fold folder ([], 1, 1) input

let isOnSprite rowLength registerVal =
    match rowLength - registerVal with
    | 1
    | 0
    | -1 -> true
    | _ -> false

let renderFolder (state: list<string> * list<char>) cycleRegister =
    let (_, regValue) = cycleRegister
    let (rows, currentRow) = state
    let rowLength = List.length currentRow
    let pixel = if (isOnSprite (rowLength) regValue) then '#' else '.'

    if rowLength = 39 then
        let newRow = new string (Array.ofList (currentRow @ [ pixel ]))
        (rows @ [ newRow ], [])
    else
        (rows, currentRow @ [ pixel ])

List.iter (fun (c, r) -> printfn " cycle : %d reg: %d" c r) result
let (rows, _) = List.fold renderFolder ([], []) ((List.rev result))
List.iter (fun x -> printfn "%s" x) rows
let signalSum = List.map mapToRecordingCycle result |> List.sum
printfn "Signal is %d" signalSum
