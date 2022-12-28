// For more information see https://aka.ms/fsharp-console-apps

let input = System.IO.File.ReadLines("input.txt")

type Position = { X: int; Y: int }
type Rope = { Head: Position; Tail: Position }

let move origin letter =
    match letter with
    | "R" -> { X = origin.X + 1; Y = origin.Y }
    | "L" -> { X = origin.X - 1; Y = origin.Y }
    | "U" -> { X = origin.X; Y = origin.Y + 1 }
    | "D" -> { X = origin.X; Y = origin.Y - 1 }
    | _ -> origin


let moveRope direction (r: Rope)(knotCount:int) =
    let head = move r.Head direction

    let xDiff = head.X - r.Tail.X
    let yDiff = head.Y - r.Tail.Y
  //  let rippleFolder (prevPos:Position) =  

    match (xDiff, yDiff) with
    | (2, _) ->
        { Head = head
          Tail = { X = head.X - 1; Y = head.Y } }
    | (-2, _) ->
        { Head = head
          Tail = { X = head.X + 1; Y = head.Y } }
    | (_, 2) ->
        { Head = head  
          Tail = { X = head.X; Y = head.Y - 1 } }
    | (_, -2) ->
        { Head = head
          Tail = { X = head.X; Y = head.Y + 1 } }
    | (_, _) -> { Head = head; Tail = r.Tail }

let folder (state: (List<Position>) * Rope) (line: string) =
    let parts = line.Split(' ')
    let direction = parts[0]
    let displacement = int parts[1]
    let (points, rope) = state

    let ropes =
        [ 0..displacement-1 ]
        |> List.scan (fun (state: Rope) _ -> (moveRope direction state 1)) rope

    let tlPoints = List.map (fun x -> x.Tail) ropes 
    (points @ tlPoints, List.last ropes)



let result =
    Seq.fold
        folder
        ([],
         { Head = { X = 0; Y = 0 }
           Tail = { X = 0; Y = 0 } })
        input

let count = List.length (fst result |> List.distinct)
List.iter (fun x -> printfn "X:%d Y:%d" x.X x.Y) (fst result |> List.distinct)




printfn "Count = %d" count
