let input = System.IO.File.ReadLines("input.txt")
type Position = { X: int; Y: int }
type Rope = { Head: Position; Tail: Position }

let printPos position = printfn "X:%d Y:%d" position.X position.Y
let move origin letter =
    match letter with
    | "R" -> { X = origin.X + 1; Y = origin.Y }
    | "L" -> { X = origin.X - 1; Y = origin.Y }
    | "U" -> { X = origin.X; Y = origin.Y + 1 }
    | "D" -> { X = origin.X; Y = origin.Y - 1 }
    | _ -> origin

let rec adjustTail (rope:list<Position>*list<Position>) =

    match fst rope with
    | hd :: hd2 :: tl ->
        let xDiff = hd.X - hd2.X
        let yDiff = hd.Y - hd2.Y
        let nextKnot =
            match (xDiff, yDiff) with
            | (2, 2) -> {X = hd.X-1; Y = hd.Y-1}
            | (-2,-2) -> {X = hd.X + 1; Y = hd.Y+1}
            | (2,-2) -> {X = hd.X - 1; Y = hd.Y+1}
            | (-2,2) -> {X = hd.X + 1; Y = hd.Y-1}
            | (2, _) -> { X = hd.X - 1; Y = hd.Y }
            | (-2, _) -> { X = hd.X + 1; Y = hd.Y }
            | (_, 2) -> { X = hd.X; Y = hd.Y - 1 }
            | (_, -2) -> { X = hd.X; Y = hd.Y + 1 }
            | (_, _) -> hd2

        adjustTail ((nextKnot :: tl), (snd rope) @ [hd] )
    | [ last ] -> (snd rope) @ [last]
    | [] -> raise (invalidArg "knots" "empty list provided")

let moveRopeBOuter direction (rope:list<Position>) = 
   match rope with
        | hd::tl -> 
            let head = move hd direction
            adjustTail (head::tl, [])
        | [] -> raise (invalidArg "rope" "should nae be empty")



let moveRope direction (r: Rope) =
    let head = move r.Head direction

    let xDiff = head.X - r.Tail.X
    let yDiff = head.Y - r.Tail.Y
  
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
        |> List.scan (fun (state: Rope) _ -> (moveRope direction state )) rope

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

let folderB (state:list<Position>*list<Position>) (input:string)=
    let parts = input.Split(' ')
    let direction = parts[0]
    let displacement = int parts[1]
    let (points, rope) = state
    let ropeArrangements =  ([1..displacement] |> List.scan (fun  state _->  (moveRopeBOuter direction state)) rope )

    (points @  List.map List.last ropeArrangements, List.last ropeArrangements)

    

let (visitedRopes,_) = Seq.fold folderB ([],List.replicate 10 {X=0;Y=0}) input 
printfn "count %d" (List.length (List.distinct visitedRopes))
printfn "Count = %d" count
