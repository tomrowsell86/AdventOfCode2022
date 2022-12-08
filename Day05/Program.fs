// For more information see https://aka.ms/fsharp-console-apps
type Instruction =
    { CrateCount: int
      Source: int
      Dest: int }

type CrateStackState =
    { Below: list<char>
      Above: list<char>
      Selection: list<char> }

let input = System.IO.File.ReadLines("input.txt") //|> Seq.map (fun x -> x.ToCharArray())

let crateInputSection: seq<char[]> =
    Seq.map (fun (x: string) -> x.ToCharArray()) input
    |> Seq.takeWhile (fun x -> Array.head (Array.skip 1 x) <> '1')

let mapLineToCrates (line: char[]) =
    Array.skip 1 line
    |> Array.indexed
    |> Array.filter (fun x -> (fst x) % 4 = 0)
    |> Array.map snd
    |> Array.indexed
    |> Array.filter (fun z -> (snd z) <> ' ')
    |> Array.map (fun x -> ((fst x) + 1, snd x))

let instructionParse (line: string) =
    let splitLines =
        line.Split(' ')
        |> Array.indexed
        |> Array.filter (fun x -> fst x = 1 || fst x = 3 || fst x = 5)
        |> Array.map snd
        |> Array.map int

    { CrateCount = splitLines[0]
      Source = splitLines[1]
      Dest = splitLines[2] }

let crateMap =
    Seq.map mapLineToCrates crateInputSection
    |> Seq.concat
    |> List.ofSeq
    |> List.groupBy (fun x -> fst x)

let instructionInput = Seq.skipWhile (fun x -> x <> "") input |> Seq.skip 1

let instructions = Seq.map instructionParse instructionInput
Seq.iter (fun x -> printfn "%d %d %d" x.CrateCount x.Source x.Dest) instructions

let instructionReader (orderSwitch: bool) (state: list<int * list<int * char>>) (i: Instruction) =
    let sourceStack = List.where (fun x -> fst x = i.Source) state |> List.head
    let destStack = List.where (fun x -> fst x = i.Dest) state |> List.head

    let grabbedItems =
        List.take i.CrateCount (snd sourceStack)
        |> if orderSwitch then List.rev else List.map (fun x-> x)
        |> List.map (fun x -> (i.Dest, snd x))

    let newSourceStack = (i.Source, List.skip i.CrateCount (snd sourceStack))
    let newDestStack = (i.Dest, grabbedItems @ (snd destStack))
    let newState = List.except [ sourceStack; destStack ] state
    let resultList = (newSourceStack :: newDestStack :: newState)
    List.iter (fun x -> List.iter (fun z -> printfn "%d %c" (fst z) (snd z)) (snd x)) resultList
    printfn "----------------------------------"
    resultList

let moveThemCrates (is9001:bool)  =
    Seq.fold (instructionReader is9001) crateMap instructions
    |> List.map (fun x -> (List.head (snd x)))
    |> List.sort
    |> List.map snd
    

let partB = new string (Array.ofList (moveThemCrates true))
let partA = new string (Array.ofList (moveThemCrates false))
printfn "Part A %s" partA
printfn "Part B %s" partB
