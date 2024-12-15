open System.IO

type XY = { X: int; Y: int; }
let makeXY x y = { X = x; Y = y; }

type Robot = { Pos: XY; Vel: XY; }
let makeRobot (pos: XY) (vel: XY) = { Pos = pos; Vel = vel }

let parseRobot (ln: string) =
    // p=0,4 v=3,-3
    let sp = ln.Split(' ')

    let spp = (sp[0][2..]).Split(',')
    let px = int spp[0]
    let py = int spp[1]

    let vpp = (sp[1][2..]).Split(',')
    let vx = int vpp[0]
    let vy = int vpp[1]

    makeRobot (makeXY px py) (makeXY vx vy)

let moveRobot (spaceWidth: int) (spaceHeight: int) (time: int) (r: Robot) =
    let mutable newX = r.Pos.X + (r.Vel.X * time)
    if newX >= 0 then
        newX <- newX % spaceWidth
    elif newX % spaceWidth = 0 then
        newX <- 0
    else
        newX <- spaceWidth + (newX % spaceWidth)

    let mutable newY = r.Pos.Y + (r.Vel.Y * time)
    if newY >= 0 then
        newY <- newY % spaceHeight
    elif newY % spaceHeight = 0 then
        newY <- 0
    else
        newY <- spaceHeight + (newY % spaceHeight)

    makeRobot (makeXY newX newY) r.Vel

let printRobots (spaceWidth: int) (spaceHeight: int) (robots: Robot list) =
    let occupiedPositions = Seq.map (fun r -> r.Pos) robots |> Set.ofSeq

    for y in 0 .. spaceHeight - 1 do
        for x in 0 .. spaceWidth - 1 do
            if Set.contains (makeXY x y) occupiedPositions then
                printf "X"
            else
                printf " "

        printfn ""


let robots =
    File.ReadAllLines("input.txt")
    |> Seq.map parseRobot
    |> List.ofSeq

// Sample
//let spaceWidth = 11
//let spaceHeight = 7
// Real
let spaceWidth = 101
let spaceHeight = 103

let robotsAfter100Secs =
    robots
    |> Seq.map (fun r -> moveRobot spaceWidth spaceHeight 100 r)
    |> List.ofSeq

let q1 = robotsAfter100Secs |> Seq.filter (fun r -> r.Pos.X < spaceWidth / 2 && r.Pos.Y < spaceHeight / 2) |> Seq.length
let q2 = robotsAfter100Secs |> Seq.filter (fun r -> r.Pos.X > spaceWidth / 2 && r.Pos.Y < spaceHeight / 2) |> Seq.length
let q3 = robotsAfter100Secs |> Seq.filter (fun r -> r.Pos.X < spaceWidth / 2 && r.Pos.Y > spaceHeight / 2) |> Seq.length
let q4 = robotsAfter100Secs |> Seq.filter (fun r -> r.Pos.X > spaceWidth / 2 && r.Pos.Y > spaceHeight / 2) |> Seq.length

printfn $"Part 1: {q1 * q2 * q3 * q4}"
