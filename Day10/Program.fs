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
    //cycles are being emitted after they have ran so need to -1 to get the in prgress reg value for a given cycle
    let ranges = [ 19..40..219 ]


    let recordedCycleResult =
        List.tryPick (fun x -> if x = cycle then Some(x) else None) ranges

    if recordedCycleResult = None then
        0
    else
        (recordedCycleResult.Value + 1) * register

let folder (state: List<int * int> * int) (line: string) =
    let (registerCycleMap, currentCycle) = state
    let lastRecording = List.tryHead registerCycleMap

    let registerVal =
        if lastRecording = None then
            1
        else
            let (_, reg) = lastRecording.Value
            reg

    match (line) with
    | NoOp -> (((currentCycle, registerVal) :: registerCycleMap), currentCycle + 1)
    
    | Add op ->
        ([ (currentCycle + 1, registerVal + op); ((currentCycle, registerVal)) ]
         @ registerCycleMap,
         currentCycle + 2)

let (result, _) = Seq.fold folder ([], 1) input
List.iter (fun (c, r) -> printfn " cycle : %d reg: %d" c r) result
let signalSum = List.map mapToRecordingCycle result |> List.sum
printfn "Signal is %d" signalSum
