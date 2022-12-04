// For more information see https://aka.ms/fsharp-console-apps
type Range = {
    Lower:int;Upper:int
}
let lines = System.IO.File.ReadLines("input.txt")
let mapToUpperLowerBound (line:string) =  
    let [|r1;r2|] = line.Split(',') 
    let [|lower1: int;upper1;|] = r1.Split('-')  |> Array.map int
    let [|lower2;upper2|] = r2.Split('-')  |> Array.map int
    ({Lower = lower1;Upper = upper1},{Lower = lower2; Upper = upper2})     

let overlapChecker r1 r2 = r1.Lower >= r2.Lower && r1.Upper <= r2.Upper 
let partBOverlapChecker r1 r2 = r1.Lower >= r2.Lower && r1.Lower <= r2.Upper 
let overLapFolder checkFn (state: int) input =
    let (r1,r2) = input
    printfn "Range u %d Range l %d "  r1.Upper r1.Lower
    printfn "Range u %d Range l %d "  r2.Upper r2.Lower
    if checkFn r1 r2 || checkFn r2 r1 then
        state + 1
    else 
        state
    
let result = Seq.map mapToUpperLowerBound lines |>  Seq.fold (overLapFolder overlapChecker) 0    

let resultB = Seq.map mapToUpperLowerBound lines |>  Seq.fold (overLapFolder partBOverlapChecker) 0    
printfn "Result %d" result

printfn "Result b %d" resultB
