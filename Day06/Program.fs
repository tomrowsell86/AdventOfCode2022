// For more information see https://aka.ms/fsharp-console-apps
open partB

let input (windowSize) =
    System.IO.File.ReadAllText("input.txt").ToCharArray()
    |> List.ofArray
    |> List.indexed
    |> List.windowed windowSize


let markerAcc (state: list<list<int * char>>) (letters: List<int * char>) =
    if uniqueChecker (List.map snd letters) then letters :: state else state

let result =
    List.fold markerAcc List.empty (input 14)
    |> List.rev
    |> List.head

    |> Array.ofList

let letters = new string ((Array.map snd result))
Array.iter (fun x -> printfn "%c %d" (snd x) (fst x)) result
let startIndex = fst (Array.last result) + 1
printfn "result = %s %d" letters startIndex 
