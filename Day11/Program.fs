open System.IO

let rec simulateBlinks blinks (stone: int64) =
    if blinks = 0 then
        1L
    elif stone = 0 then
        let newStone = 1
        simulateBlinks (blinks - 1) newStone
    elif stone.ToString().Length % 2 = 0 then
        let stoneString = stone.ToString()
        let newStone1 = int64 stoneString[0 .. stoneString.Length / 2 - 1]
        let newStone2 = int64 stoneString[stoneString.Length / 2 ..]
        simulateBlinks (blinks - 1) newStone1 + simulateBlinks (blinks - 1) newStone2
    else
        simulateBlinks (blinks - 1) (stone * 2024L)


let initialStones =
    File.ReadAllLines("input.txt")[0]
    |> (fun ln -> ln.Split())
    |> Seq.map int64
    |> Array.ofSeq


let numStonesPart1 =
    Seq.map (simulateBlinks 25) initialStones
    |> List.ofSeq

printfn $"Part 1: {Seq.sum numStonesPart1}"


//let numStonesPart2 =
//    Seq.map (simulateBlinks 75) initialStones
//    |> List.ofSeq

//printfn $"Part 2: {Seq.sum numStonesPart2}"
