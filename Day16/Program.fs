open System.Collections.Generic
open System.IO

type Dir = | Up | Right | Down | Left

type Placement = { X: int; Y: int; Dir: Dir; AccCost: int; }

let map =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun ln -> Array.ofSeq ln)
    |> Array.ofSeq

let height = map.Length
let width = map[0].Length

let isFinished (p: Placement) = map[p.Y][p.X] = 'E'

let startPos =
    Seq.allPairs { 0 .. width - 1 } { 0 .. height - 1 }
    |> Seq.find (fun (x, y) -> map[y][x] = 'S')

let queue = new PriorityQueue<Placement, int>()
let visited = new HashSet<int * int * Dir>()
let initialPlacement = { X = (fst startPos); Y = (snd startPos); Dir = Dir.Right; AccCost = 0; }
queue.Enqueue(initialPlacement, 0)

while not (isFinished (queue.Peek())) do
    let p = queue.Dequeue()
    visited.Add((p.X, p.Y, p.Dir)) |> ignore

    if p.Dir <> Right && p.X > 0 && map[p.Y][p.X - 1] <> '#' && not (visited.Contains((p.X - 1, p.Y, Left))) then
        if p.Dir <> Left then
            let newPlacement = { p with Dir = Left; AccCost = p.AccCost + 1000 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)
        else
            let newPlacement = { p with X = p.X - 1; AccCost = p.AccCost + 1 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)

    if p.Dir <> Left && p.X < width - 1 && map[p.Y][p.X + 1] <> '#' && not (visited.Contains((p.X + 1, p.Y, Right))) then
        if p.Dir <> Right then
            let newPlacement = { p with Dir = Right; AccCost = p.AccCost + 1000 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)
        else
            let newPlacement = { p with X = p.X + 1; AccCost = p.AccCost + 1 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)

    if p.Dir <> Down && p.Y > 0 && map[p.Y - 1][p.X] <> '#' && not (visited.Contains((p.X, p.Y - 1, Up))) then
        if p.Dir <> Up then
            let newPlacement = { p with Dir = Up; AccCost = p.AccCost + 1000 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)
        else
            let newPlacement = { p with Y = p.Y - 1; AccCost = p.AccCost + 1 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)

    if p.Dir <> Up && p.Y < height - 1 && map[p.Y + 1][p.X] <> '#' && not (visited.Contains((p.X, p.Y + 1, Down))) then
        if p.Dir <> Down then
            let newPlacement = { p with Dir = Down; AccCost = p.AccCost + 1000 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)
        else
            let newPlacement = { p with Y = p.Y + 1; AccCost = p.AccCost + 1 }
            queue.Enqueue(newPlacement, newPlacement.AccCost)
    
let finalMove = queue.Dequeue()
printfn $"Part 1: {finalMove.AccCost}"
