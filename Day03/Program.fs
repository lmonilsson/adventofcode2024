open System.IO
open System.Text.RegularExpressions

let part1 input =
    let matches = Regex.Matches(input, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)")
    let result =
        matches
        |> Seq.map (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value))
        |> Seq.sum

    printfn $"Part 1: {result}"


let part2 input =
    let matches = Regex.Matches(input, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\)")

    let mutable active = true
    let mutable result = 0
    for m in matches do
        match m.Value with
        | "do()" -> active <- true
        | "don't()" -> active <- false
        | _ ->
            if active then
                result <- result + ((int m.Groups[1].Value) * (int m.Groups[2].Value))

    printfn $"Part 2: {result}"


// Entry point
let input = File.ReadAllText("input.txt")
part1 input
part2 input
