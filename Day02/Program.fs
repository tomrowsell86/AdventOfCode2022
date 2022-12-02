// For more information see https://aka.ms/fsharp-console-apps

let input = System.IO.File.ReadLines("input.txt")
let scoreObject turn = 
    match turn with
        | ("A"|"X") -> 1
        | ("B" |"Y") -> 2
        | ("C" |"Z") -> 3
        | _ -> 0
let determineScore p1 p2 = 
    let p2ObjectScore = scoreObject p2
    let p1ObjectScore = scoreObject p1
    if p1ObjectScore = p2ObjectScore then
        (3 + p1ObjectScore,3 + p2ObjectScore)
    else if (p1ObjectScore= 2 && p2ObjectScore= 1)|| (p1ObjectScore=3  && p2ObjectScore = 2)   || (p1ObjectScore = 1 && p2ObjectScore = 3) then
        (6 + p1ObjectScore,p2ObjectScore)
    else
        (p1ObjectScore,6 + p2ObjectScore)

let shapeMap = Map[("A", "X"); ("B", "Y");("C", "Z")]

let determineShape p1 outcome= 
    if outcome = "Y" then
        Map.find p1 shapeMap
    else if outcome = "X" then
        if p1 = "A" then
            "Z"
        else if p1 = "B" then
            "X"
        else 
            "Y"
    else
        if p1 = "A" then
            "Y"
        else if p1="B" then
            "Z"
        else
            "X"

let folder (partB:bool) (state:int*int) (turn:string) =
    let [|p1;p2|] = turn.Split(' ')
    let roundScore = 
        if partB then
            let p = determineScore p1 (determineShape p1 p2)
            printfn "%d %d" (fst p) (snd p)
            p
        else
            determineScore p1 p2 
    ((fst state) + (fst roundScore), (snd state) + (snd roundScore))

let calculationPartA = Seq.fold (folder false) (0,0) input
let calculationPartB = Seq.fold (folder true) (0,0) input 
printfn "Result for p2 is %d" (snd calculationPartA)
printfn "Result for part b is %d" (snd calculationPartB)