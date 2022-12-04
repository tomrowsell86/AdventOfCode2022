// For more information see https://aka.ms/fsharp-console-apps
let input = System.IO.File.ReadLines("input.txt")
let splitLine (line:char[]) = 
    (Array.take((Array.length line) / 2) line,
    Array.skip((Array.length line) / 2 ) line )

let priorityConvert (c:char) = 
    let asciiCode = int c
    if asciiCode >= 65 && asciiCode <= 90 then
        asciiCode - 38
    else
        asciiCode - 96

let duplicateFilter (c1:char[]) (c2:char[]) =
         Array.map (fun s-> (Array.contains s c2),s)  c1
        |> Array.where (fun x -> fst x) |> Array.map snd
let folder (state:int) (input:string) = 
    let (c1,c2) = splitLine (input.ToCharArray())
    let dupChar = duplicateFilter c1 c2 |>  Array.head 
    state + priorityConvert dupChar

let folderB (state:list<string>*int) (input:string) =
    let currentList = fst state 
    let newList = input::currentList
    if List.length newList < 3 then
        (input::fst state,snd state)
    else
        List.iter (fun x -> printfn "string %s" x) newList
        match newList with 
        | [a;b;c]-> 
            let dupChar = ((duplicateFilter (duplicateFilter (a.ToCharArray()) (b.ToCharArray())) (c.ToCharArray()) |> Array.head))
            (List.empty, (snd state) + priorityConvert dupChar)
        | _ -> raise (System.InvalidOperationException("Could nae match"))

        
       // duplicateFilter (duplicateFilter List.indexed 

let result = Seq.fold folder 0 input
let resultB = Seq.fold folderB (List.Empty,0) input
printfn "Part A result %d" result

printfn "Part b result %d" (snd resultB)

