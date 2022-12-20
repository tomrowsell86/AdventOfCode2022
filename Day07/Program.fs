type Dir =
    { Name: string
      Size: int }
let input = System.IO.File.ReadLines("input.txt")

let terminalFolder (state: List<Dir>*List<Dir>) (line: string) : List<Dir>*List<Dir> =
    let parts = line.Split(' ')


    if parts[0] <> "$" then
        if parts[0] <> "dir" then
            (List.map (fun x -> {Name = x.Name; Size = x.Size + int parts[0]}) (fst state),snd state)
        else
            state
    else if parts[1] = "cd" then
        if parts[2] = ".." then
            match fst state with
            | hd::tl -> 
                (tl, hd::(snd state))
            | [_]|[] -> state 
        else if parts[2] = "/" then
            state
        else
            ({Name = parts[2]; Size = 0}::fst state, snd state)
    else
        state

let root = { Name = "/"; Size = 0 }
let result =  (Seq.fold terminalFolder ([root], []) input) 
let combinedResult = fst result@snd result 

//List.iter (fun x -> printfn "%d %s" x.Size x.Name)  (fst result@snd result)
let total = (List.where (fun x -> x.Size <= 100000) >>  List.sumBy (fun x -> x.Size))combinedResult
let rootNode = (List.where (fun x -> x.Name = "/") >> List.head) combinedResult 
let unusedSpace = 70000000 - rootNode.Size
let requiredSpace = 30000000 - unusedSpace
let spaceyDirs = List.where (fun x -> x.Size >= requiredSpace) combinedResult
let minDir = List.minBy (fun x -> x.Size) spaceyDirs
List.iter (fun x -> printfn "%d %s" x.Size x.Name) spaceyDirs
printfn "unused space %d" (requiredSpace)
printfn "littlest dir %d" minDir.Size
//printfn "%d" total

