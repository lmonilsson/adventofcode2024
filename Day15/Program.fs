open System
open System.IO

(*
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
*)

type XY = { X: int; Y: int; }
let makeXY x y = { X = x; Y = y; }

let allCoords w h =
    Seq.allPairs { 0 .. w - 1 } { 0 .. h - 1 }
    |> Seq.map (fun p -> makeXY (fst p) (snd p))

let printMap (map: char array array) =  
    for y in 0 .. map.Length - 1 do
        for x in 0 .. map[0].Length - 1 do
            printf $"{map[y][x]}"
        printfn ""

let input = File.ReadAllLines("input.txt")

let mutable map =
    input
    |> Seq.takeWhile (fun ln -> ln.Length > 0)
    |> Seq.map (fun ln -> Array.ofSeq ln)
    |> Array.ofSeq

let mapHeight = map.Length
let mapWidth = map[0].Length

let moveLines = input |> Seq.skipWhile (fun ln -> ln.Length > 0) |> Seq.skip 1
let moves = String.Join("", moveLines)

let mutable robot =
    allCoords mapWidth mapHeight
    |> Seq.find (fun p -> map[p.Y][p.X] = '@')

for i in 0 .. moves.Length - 1 do
    //printMap map
    //printfn $"{moves[i]}"
    match moves[i] with
    | '<' ->
        let nextX =
            { robot.X - 1 .. -1 .. 0 }
            |> Seq.find (fun x -> map[robot.Y][x] = '#' || map[robot.Y][x] = '.')

        if map[robot.Y][nextX] = '.' then
            { nextX .. robot.X - 1 } |> Seq.iter (fun x -> map[robot.Y][x] <- map[robot.Y][x + 1])
            map[robot.Y][robot.X] <- '.'
            robot <- { robot with X = robot.X - 1 }

    | '>' ->
        let nextX =
            { robot.X + 1 .. mapWidth - 1 }
            |> Seq.find (fun x -> map[robot.Y][x] = '#' || map[robot.Y][x] = '.')

        if map[robot.Y][nextX] = '.' then
            { nextX .. -1 .. robot.X + 1 } |> Seq.iter (fun x -> map[robot.Y][x] <- map[robot.Y][x - 1])
            map[robot.Y][robot.X] <- '.'
            robot <- { robot with X = robot.X + 1 }

    | '^' ->
        let nextY =
            { robot.Y - 1 .. -1 .. 0 }
            |> Seq.find (fun y -> map[y][robot.X] = '#' || map[y][robot.X] = '.')

        if map[nextY][robot.X] = '.' then
            { nextY .. robot.Y - 1 } |> Seq.iter (fun y -> map[y][robot.X] <- map[y + 1][robot.X])
            map[robot.Y][robot.X] <- '.'
            robot <- { robot with Y = robot.Y - 1 }

    | 'v' ->
        let nextY =
            { robot.Y + 1 .. mapHeight - 1 }
            |> Seq.find (fun y -> map[y][robot.X] = '#' || map[y][robot.X] = '.')

        if map[nextY][robot.X] = '.' then
            { nextY .. -1 .. robot.Y + 1 } |> Seq.iter (fun y -> map[y][robot.X] <- map[y - 1][robot.X])
            map[robot.Y][robot.X] <- '.'
            robot <- { robot with Y = robot.Y + 1 }

    | _ -> failwith $"Unexpected movement {moves[i]} at index {i}"

printMap map

let gpsSum =
    allCoords mapWidth mapHeight
    |> Seq.filter (fun p -> map[p.Y][p.X] = 'O')
    |> Seq.sumBy (fun p -> p.Y * 100 + p.X)

printfn $"Part 1: {gpsSum}"
