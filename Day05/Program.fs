open System
open System.IO

let middleElement (u: int list) = u.[(List.length u) / 2]

let comesBefore (rules: (int*int) list) p1 p2 =
    Seq.tryFind (fun (a, b) -> a = p1 && b = p2) rules
    |> _.IsSome

let isUpdateValid (rules: (int*int) list) (pages: int list) =
    let invalidPages =
        Seq.filter
            (fun (i, p1) ->
                let laterPages = Seq.skip (i + 1) pages
                let wrongOrder = Seq.tryFind (fun p2 -> comesBefore rules p2 p1) laterPages
                wrongOrder.IsSome)
            (Seq.indexed pages)

    Seq.isEmpty invalidPages

let sortPages (rules: (int*int) list) (pages: int list) =
    Seq.sortWith (fun p1 p2 -> if comesBefore rules p1 p2 then -1 else 1) pages
    |> List.ofSeq

let part1 (rules: (int*int) list) (updates: int list list) =
    let validUpdates = updates |> Seq.filter (isUpdateValid rules)
    let middleNumbers = Seq.map middleElement validUpdates
    printfn $"Part 1: {Seq.sum middleNumbers}"

let part2 (rules: (int*int) list) (updates: int list list) =
    let invalidUpdates = updates |> Seq.filter (not << (isUpdateValid rules))
    let fixedUpdates = invalidUpdates |> Seq.map (fun u -> sortPages rules u)
    let middleNumbers = Seq.map middleElement fixedUpdates
    printfn $"Part 2: {Seq.sum middleNumbers}"


// Entry point
let lines = File.ReadAllLines "input.txt" |> List.ofArray

// List of tuples of page numbers (comes-before, comes-after).
let rules =
    Seq.takeWhile (fun ln -> not (String.IsNullOrEmpty(ln))) lines
    |> Seq.map (fun ln -> ln.Split("|"))
    |> Seq.map (fun x -> int x[0], int x[1])
    |> List.ofSeq

// List of updates, each being a list of page numbers.
let updates =
    Seq.skipWhile (fun ln -> not (String.IsNullOrEmpty(ln))) lines
    |> Seq.skip 1
    |> Seq.map (fun ln -> ln.Split(","))
    |> Seq.map (fun strs -> Seq.map int strs |> List.ofSeq )
    |> List.ofSeq

part1 rules updates
part2 rules updates
