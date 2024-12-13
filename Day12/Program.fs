open System.Collections.Generic
open System.IO
open System.Linq

type Map = char array array
type Pos = { Y: int; X: int; }

let surrounds (height: int) (width: int) (p: Pos) =
    seq {
        { Y = p.Y - 1; X = p.X    ; };
        { Y = p.Y + 1; X = p.X    ; };
        { Y = p.Y    ; X = p.X - 1; };
        { Y = p.Y    ; X = p.X + 1; };
    }
    |> Seq.filter (fun p -> p.Y >= 0 && p.Y < height && p.X >= 0 && p.X < width)

let rec expandRegion (map: Map) (start: Pos) (plant: char) (collected: HashSet<Pos>) =
    if not (collected.Contains(start)) then
        collected.Add(start) |> ignore

        surrounds map.Length map[0].Length start
        |> Seq.filter (fun p -> map[p.Y][p.X] = plant)
        |> Seq.iter (fun p -> expandRegion map p plant collected)

let findRegions (map: Map) =
    let mutable unvisitedPositions =
        new HashSet<Pos>(
            Seq.allPairs { 0 .. map.Length - 1 } { 0 .. map[0].Length - 1 }
            |> Seq.map (fun (y, x) -> { Y = y; X = x; }))

    let mutable regions: Pos list list = List.empty

    while unvisitedPositions.Any() do
        let start = unvisitedPositions.First()
        let plant = map[start.Y][start.X]
        let regionSet = new HashSet<Pos>()
        expandRegion map start plant regionSet
        let region = (List.ofSeq regionSet)
        regions <- region :: regions
        unvisitedPositions.ExceptWith(region)

    regions

let isNeighbour (p1: Pos) (p2: Pos) =
    p1.Y = p2.Y && abs(p1.X - p2.X) = 1 ||
    p1.X = p2.X && abs(p1.Y - p2.Y) = 1

let calcPerimeter (region: Pos list) =
    Seq.sumBy (fun p1 -> 
        let neighbours = Seq.filter (fun p2 -> isNeighbour p1 p2) region |> Seq.length
        4 - neighbours
    ) region

let calculatePriceFromAreaAndPerimeter (region: Pos list) =
    let area = region.Length
    let perimeter = calcPerimeter region
    area * perimeter
    
let calculatePriceFromAreaAndSides (region: Pos list) =
    let area = region.Length
    let sides = 42
    area * sides


let map = File.ReadAllLines("input.txt") |> Seq.map Array.ofSeq |> Array.ofSeq
let regions = findRegions map
printfn $"Part 1: {Seq.sumBy calculatePriceFromAreaAndPerimeter regions}"
printfn $"Part 2: {Seq.sumBy calculatePriceFromAreaAndSides regions}"
