open System
open System.IO

let part1 (column1: int List, column2: int List): int =
    let sortedColumn1 = Seq.sort column1
    let sortedColumn2 = Seq.sort column2
    let diffs = Seq.map2 (fun a b -> abs (a - b)) sortedColumn1 sortedColumn2
    Seq.sum diffs

let part2 (column1: int List, column2: int List): int =
    let column2Freqs = Seq.countBy (fun n -> n) column2
    let column1Counts =
        Seq.map (fun n -> Seq.tryFind (fun kv -> (fst kv) = n) column2Freqs) column1
        |> Seq.filter _.IsSome
        |> Seq.map _.Value
    Seq.map (fun kv -> (fst kv) * (snd kv)) column1Counts |> Seq.sum

[<EntryPoint>]
let main argv =
    let numbers =
        File.ReadAllText "input.txt"
        |> _.Split([|' '; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map Int32.Parse
        |> List.ofSeq

    let column1 = List.map (fun i -> numbers[i]) [0 .. 2 .. numbers.Length - 2]
    let column2 = List.map (fun i -> numbers[i]) [1 .. 2 .. numbers.Length - 1]

    printfn $"Part 1: {part1 (column1, column2)}"
    printfn $"Part 2: {part2 (column1, column2)}"
    0
