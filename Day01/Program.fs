// For more information see https://aka.ms/fsharp-console-apps
let input = System.IO.File.ReadLines("input.txt")

let topNCompare (l:list<int>) (n:int) (compareValue:int) = (List.sortDescending (compareValue::l)) |> List.take n  


let folder (top:int) (state: list<int>*list<int>) (x:string)  : list<int>*list<int> = 
    if x = "" then
        let currentSum = List.sum (fst state)
        let topList = topNCompare (snd state) top  currentSum
        (List.empty,topList)
    else
        let newList =  (int x)::(fst state)  
        (newList,snd state)

let partAResult = Seq.fold (folder 1) (List.empty,[0]) input
let partBResult = Seq.fold (folder 3) (List.empty,[0;0;0]) input 
printfn "The highest calorie count is %d" (List.sum (snd partAResult)) 
printfn "The highest calorie count is %d" (List.sum (snd partBResult)) 
