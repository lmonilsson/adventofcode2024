open System.IO

(*

Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

*)

type XY = { X: int64; Y: int64; }
type Machine = { ButtonA: XY; ButtonB: XY; PrizeAt: XY; }

let parseButton (buttonString: string) =
    // Button A: X+10, Y+22
    let sp = buttonString.Split(',')
    let x = int (sp[0].Split('+')[1])
    let y = int (sp[1].Split('+')[1])
    { X = x; Y = y; }

let parsePrize (prizeString: string) =
    // Prize: X=100, Y=200
    let sp = prizeString.Split(',')
    let x = int (sp[0].Split('=')[1])
    let y = int (sp[1].Split('=')[1])
    { X = x; Y = y; }

let parseMachine (lines: string array) =
    let buttonA = parseButton lines[0]
    let buttonB = parseButton lines[1]
    let prizeAt = parsePrize lines[2]
    { ButtonA = buttonA; ButtonB = buttonB; PrizeAt = prizeAt; }

let doesComplete (machine: Machine) (a: int64) (b: int64) =
    let x = machine.ButtonA.X * a + machine.ButtonB.X * b
    let y = machine.ButtonA.Y * a + machine.ButtonB.Y * b
    x = machine.PrizeAt.X && y = machine.PrizeAt.Y

let cost (a, b) = a * 3 + b 

let fewestTokenToComplete (machine: Machine) =
    let completedCosts =
        Seq.allPairs { 1 .. 100 } { 1.. 100 }
        |> Seq.filter (fun (a, b) -> doesComplete machine a b)
        |> Seq.map cost

    if Seq.isEmpty completedCosts then
        None
    else
        Some (Seq.min completedCosts)

let machines =
    File.ReadAllLines("input_sample.txt")
    |> Seq.chunkBySize 4
    |> Seq.map parseMachine
    |> List.ofSeq

let fewestTokensPerMachine =
    Seq.choose fewestTokenToComplete machines
    |> List.ofSeq

printfn $"Part 1: {Seq.sum fewestTokensPerMachine}"


//let correctedMachines =
//    machines
//    |> Seq.map (fun m -> {
//        ButtonA = m.ButtonA;
//        ButtonB = m.ButtonB;
//        PrizeAt = { X = m.PrizeAt.X + 10000000000000L; Y = m.PrizeAt.Y + 10000000000000L; }
//    })
//    |> List.ofSeq

//let fewestTokensPerCorrectedMachine =
//    Seq.choose fewestTokenToComplete correctedMachines
//    |> List.ofSeq

//printfn $"Part 2: {Seq.sum fewestTokensPerCorrectedMachine}"
