type TreePosition = { Height: int; X: int; Y: int }

let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun x -> (x.ToCharArray()) |> Array.indexed)
    |> Seq.indexed
    |> Seq.map (fun (a, b) ->
        Array.map
            (fun z ->
                { Height = (snd >> string >> int) z
                  X = fst z
                  Y = a })
            b)
    |> Seq.concat
    |> Array.ofSeq

let isEdgeTree tree maxX maxY =
    tree.X = 0 || tree.Y = 0 || tree.X = maxX || tree.Y = maxY

let (maxX, maxY) =
    ((Array.map (fun x -> x.Y) >> Array.max) input, (Array.map (fun x -> x.X) >> Array.max) input)

let isVisible (tree: TreePosition) (treeMap: TreePosition[]) =

    if isEdgeTree tree maxX maxY then
        true
    else
        let visibleLeft =
            (Array.where (fun x -> x.Y = tree.Y && x.X < tree.X)
             >> Array.forall (fun x -> x.Height < tree.Height))
                treeMap

        let visibleRight =
            (Array.where (fun x -> x.Y = tree.Y && x.X > tree.X)
             >> Array.forall (fun x -> x.Height < tree.Height))
                treeMap

        let visibleTop =
            (Array.where (fun x -> x.Y < tree.Y && x.X = tree.X)
             >> Array.forall (fun x -> x.Height < tree.Height))
                treeMap

        let visibleBottom =
            (Array.where (fun x -> x.Y > tree.Y && x.X = tree.X)
             >> Array.forall (fun x -> x.Height < tree.Height))
                treeMap

        visibleBottom || visibleLeft || visibleRight || visibleTop

let folderA (state: List<TreePosition> * TreePosition[]) (tree: TreePosition) =
    let map = snd state
    let visibleTrees = fst state

    if (isVisible tree map) then
        (tree :: visibleTrees, map)
    else
        state

let clearingDistance (tree: TreePosition) (treeSubset: TreePosition[]) =
    let result = Array.takeWhile (fun x -> x.Height < tree.Height) treeSubset
    let issNotEdge tree = not (isEdgeTree tree maxX maxY)
    let length = Array.length result
    let last = Array.tryLast result

    if last <> None then
        if issNotEdge last.Value then length + 1 else length
    else
        1

let folderB (state: TreePosition[] * int) (tree: TreePosition) =
    let forward =
        clearingDistance
            tree
            (Array.where (fun x -> x.X = tree.X && x.Y < tree.Y) (fst state)
             |> Array.sortByDescending (fun x -> x.Y))

    let downward =
        clearingDistance
            tree
            (Array.where (fun x -> x.X = tree.X && x.Y > tree.Y) (fst state)
             |> Array.sortBy (fun x -> x.Y))

    let left =
        clearingDistance
            tree
            (Array.where (fun x -> x.X < tree.X && x.Y = tree.Y) (fst state)
             |> Array.sortByDescending (fun x -> x.X))

    let right =
        clearingDistance
            tree
            (Array.where (fun x -> x.X > tree.X && x.Y = tree.Y) (fst state)
             |> Array.sortBy (fun x -> x.X))

    let scenicScore = forward * downward * left * right
    printfn "f:%d b:%d l:%d r:%d  scenic score %d  x:%d y%d" forward downward left right scenicScore tree.X tree.Y

    if scenicScore > snd state then
        (fst state, scenicScore)
    else
        state

let result = (Array.fold folderA ([], input) >> fst) input

let resultB =
    (Array.where (fun x -> not (isEdgeTree x maxX maxY))
     >> Array.fold folderB (input, 0)
     >> snd)
        input

let resultCount = List.length result
printfn "%d" resultCount
printfn "%d" resultB
