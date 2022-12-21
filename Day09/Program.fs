// For more information see https://aka.ms/fsharp-console-apps

let input = System.IO.File.ReadLines("input.txt")

type Position = { X: int; Y: int }
type Rope = { Head: Position; Tail: Position }
let isHorizontal (rope: Rope) = rope.Head.Y = rope.Tail.Y

let folder (state: (List<Position>) * Rope) (line: string) =
    let parts = line.Split(' ')
    let displacement = int parts[1]
    let (trail, rope) = state

    if parts[0] = "R" || parts[0] = "L" then
        let multiplier = if parts[0] = "R" then 1 else -1

        if isHorizontal rope then
            let ropeDisplacement = (rope.Head.X - rope.Tail.X)

            let tailDisplacement =
                multiplier
                * if ropeDisplacement < 0 then displacement - 2
                  else if ropeDisplacement = 0 then displacement - 1
                  else displacement

            let items =
                [ rope.Tail.X .. (rope.Tail.X + tailDisplacement - 1) ]
                |> List.map (fun x -> { X = x; Y = rope.Head.Y })

            (trail @ items,
             { Head =
                 { X = rope.Head.X + displacement
                   Y = rope.Head.Y }
               Tail =
                 { Y = rope.Tail.Y
                   X = rope.Tail.X + tailDisplacement } })
        else
            let tailX = rope.Head.X + displacement - 2
            let hdX = rope.Head.X + displacement - 1

            let points =
                [ rope.Head.X .. (tailX) ] |> List.map (fun x -> { X = x; Y = rope.Head.Y })

            (points,
             { Head = { X = hdX; Y = rope.Head.Y }
               Tail = { X = tailX; Y = rope.Head.Y } })
    else
        state


let result =
    Seq.fold
        folder
        ([],
         { Head = { X = 0; Y = 0 }
           Tail = { X = 0; Y = 0 } })
        input

List.iter (fun x -> printfn "%d %d" x.X x.Y) (fst result)
printfn "%d" ((fst >> List.length) result)
printfn "Hello from F#"
