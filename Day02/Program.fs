open System
open System.IO

let parseReport (str: string) =
    Seq.map int (str.Split(' '))
    |> List.ofSeq


let adjacentLevelsSafe increasing (a, b) =
    if increasing then
        b - a >= 1 && b - a <= 3
    else
        a - b >= 1 && a - b <= 3


let isSafeReport (report: int List): bool =
    let increasing = report[1] > report[0]
    Seq.pairwise report |> Seq.forall (adjacentLevelsSafe increasing)


let tryMakeSafe (report: int List): int list Option =
    if isSafeReport report then
        Option.Some report
    else
        // The problem is one of:
        // * The first element
        // * The second element
        // * One other element, when following the direction set by the first two elements
        // * Otherwise, it cannot be made safe
        //
        // This avoids testing all combinations by testing at most 4 removals per report.
        let withoutFirst = List.skip 1 report
        if isSafeReport withoutFirst then Option.Some withoutFirst
        else
            let withoutSecond = (List.take 1 report) @ (List.skip 2 report)
            if isSafeReport withoutSecond then Option.Some withoutSecond
            else
                let increasing = report[1] > report[0]
                let checkPair = adjacentLevelsSafe increasing
                let pairs = Seq.pairwise report
                let badPairIndex = Seq.findIndex (fun ab -> not (checkPair ab)) pairs
                let withoutOneLevel = List.take (badPairIndex + 1) report @ List.skip (badPairIndex + 2) report
                if isSafeReport withoutOneLevel then Option.Some withoutOneLevel
                else
                    let withoutOtherLevel = List.take (badPairIndex + 0) report @ List.skip (badPairIndex + 1) report
                    if isSafeReport withoutOtherLevel then Option.Some withoutOtherLevel
                    else
                        Option.None


let part1 (reports: int List List) =
    let safeReports = Seq.filter isSafeReport reports
    printfn $"Part 1: {Seq.length safeReports}"


let part2 (reports: int List List) =
    let safeReports =
        Seq.map tryMakeSafe reports
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)
    printfn $"Part 2: {Seq.length safeReports}"


[<EntryPoint>]
let main argv =
    (*
        7 6 4 2 1
        1 2 7 8 9
        9 7 6 2 1
    *)
    let reports =
        File.ReadAllLines "input.txt"
        |> Seq.map parseReport
        |> List.ofSeq
    
    part1 reports
    part2 reports

    0
