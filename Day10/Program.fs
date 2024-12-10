open System.IO

type Pos = { Y: int; X: int; }
type Map = int array array

let mapHeight (map: Map) = map.Length
let mapWidth (map: Map) = map[0].Length

let surrounds (height: int) (width: int) (p: Pos) =
    seq {
        { Y = p.Y - 1; X = p.X    ; };
        { Y = p.Y + 1; X = p.X    ; };
        { Y = p.Y    ; X = p.X - 1; };
        { Y = p.Y    ; X = p.X + 1; };
    }
    |> Seq.filter (fun p -> p.Y >= 0 && p.Y < height && p.X >= 0 && p.X < width)


let rec findTops (topoMap: Map) (head: Pos) =
    let v = topoMap[head.Y][head.X]
    if v = 9 then
        seq { head }
    else
        surrounds (mapHeight topoMap) (mapWidth topoMap) head
        |> Seq.filter (fun p -> topoMap[p.Y][p.X] = v + 1)
        |> Seq.collect (fun p -> findTops topoMap p)
        |> Seq.distinct

let part1 (topoMap: Map) (trailHeads: Pos list) =
    // Non-distinct tops. The same top can come from multiple trailheads,
    // but it cannot come multiple times from the same trailhead.
    let tops = Seq.collect (fun p -> findTops topoMap p) trailHeads

    printfn $"Part 1: {Seq.length tops}"
    
    
let rec findPathCount (topoMap: Map) (head: Pos) =
    let v = topoMap[head.Y][head.X]
    if v = 9 then
        1
    else
        surrounds (mapHeight topoMap) (mapWidth topoMap) head
        |> Seq.filter (fun p -> topoMap[p.Y][p.X] = v + 1)
        |> Seq.sumBy (fun p -> findPathCount topoMap p)

let part2 (topoMap: Map) (trailHeads: Pos list) =
    // Non-distinct tops. The same top can come from multiple trailheads,
    // but it cannot come multiple times from the same trailhead.
    let pathCounts = Seq.map (fun p -> findPathCount topoMap p) trailHeads

    printfn $"Part 1: {Seq.sum pathCounts}"


let topoMap =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun ln -> 
        Seq.map (fun c -> int c - int '0') ln
        |> Array.ofSeq)
    |> Array.ofSeq

let height = mapHeight topoMap
let width = mapWidth topoMap
let trailHeads =
    Seq.allPairs { 0 .. height - 1 } { 0 .. width - 1 }
    |> Seq.filter (fun (y, x) -> topoMap[y][x] = 0)
    |> Seq.map (fun (y, x) -> { Y = y; X = x; })
    |> List.ofSeq

part1 topoMap trailHeads
part2 topoMap trailHeads
