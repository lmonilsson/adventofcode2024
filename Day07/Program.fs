open System.IO

type Equation = { Value: int64; Parts: int64 list }

let rec testBasicOperators (target: int64) (parts: int64 List) (aggregated: int64) =
    if parts.IsEmpty then
        aggregated = target
    else
        testBasicOperators target (parts.Tail) (aggregated + parts.Head) ||
        testBasicOperators target (parts.Tail) (aggregated * parts.Head)

let canMakeValidWithBasicOperators (target: int64) (parts: int64 list) =
    testBasicOperators target parts.Tail parts.Head

let part1 (equations: Equation list) =
    let result =
        equations
        |> Seq.filter (fun eq -> canMakeValidWithBasicOperators eq.Value eq.Parts)
        |> Seq.sumBy (fun eq -> eq.Value)

    printfn $"Part 1: {result}"
    

let rec testExtendedOperators (target: int64) (parts: int64 List) (aggregated: int64) =
    if parts.IsEmpty then
        aggregated = target
    else
        testExtendedOperators target (parts.Tail) (aggregated + parts.Head) ||
        testExtendedOperators target (parts.Tail) (aggregated * parts.Head) ||
        testExtendedOperators target (parts.Tail) (int64 (aggregated.ToString() + parts.Head.ToString()))

let canMakeValidWithExtendedOperators (target: int64) (parts: int64 list) =
    testExtendedOperators target parts.Tail parts.Head

let part2 (equations: Equation list) =
    let result =
        equations
        |> Seq.filter (fun eq -> canMakeValidWithExtendedOperators eq.Value eq.Parts)
        |> Seq.sumBy (fun eq -> eq.Value)

    printfn $"Part 2: {result}"


let equations =
    File.ReadAllLines "input.txt"
    |> Seq.map (fun ln ->
        // 3267: 81 40 27
        let sp = ln.Split(": ")
        let value = int64 sp[0]
        let parts = Seq.map int64 (sp[1].Split(' ')) |> List.ofSeq
        { Value = value; Parts = parts })
    |> List.ofSeq

part1 equations
part2 equations
