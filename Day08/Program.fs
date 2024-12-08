open System.IO

type Pos = { Y: int; X: int; }

let part1 mapHeight mapWidth (positionsPerFreq: (char * Pos seq) seq) =
    let isWithin (p: Pos) = p.Y >= 0 && p.Y < mapHeight && p.X >= 0 && p.X < mapWidth

    let antinodes =
        positionsPerFreq
        |> Seq.collect (fun (_freq, positions) ->
            Seq.allPairs positions positions
            |> Seq.filter (fun (p1, p2) -> p1 <> p2 && p2.Y >= p1.Y && (p2.Y > p1.Y || p2.X > p2.X))
            |> Seq.collect (fun (p1, p2) ->
                let dy = p2.Y - p1.Y
                let dx = p2.X - p1.X
                let a1 = { Y = p1.Y - dy; X = p1.X - dx; }
                let a2 = { Y = p2.Y + dy; X = p2.X + dx; }
                Seq.filter isWithin [ a1; a2 ]))
        |> Seq.distinct

    printfn $"Part 1: {Seq.length antinodes}"

let part2 mapHeight mapWidth (positionsPerFreq: (char * Pos seq) seq) =
    let isWithin (p: Pos) = p.Y >= 0 && p.Y < mapHeight && p.X >= 0 && p.X < mapWidth

    let antinodes =
        positionsPerFreq
        |> Seq.collect (fun (_freq, positions) ->
            Seq.allPairs positions positions
            |> Seq.filter (fun (p1, p2) -> p1 <> p2 && p2.Y >= p1.Y && (p2.Y > p1.Y || p2.X > p2.X))
            |> Seq.collect (fun (p1, p2) ->
                seq {
                    // The antinodes on the antennas.
                    yield p1
                    yield p2

                    // The other antinodes. The map size is an upper bound of number of steps (not pretty).
                    for i in 1 .. (max mapHeight mapWidth) do
                        let dy = (p2.Y - p1.Y) * i
                        let dx = (p2.X - p1.X) * i
                        let a1 = { Y = p1.Y - dy; X = p1.X - dx; }
                        let a2 = { Y = p2.Y + dy; X = p2.X + dx; }
                        if isWithin a1 then
                            yield a1
                        if isWithin a2 then
                            yield a2
                }))
        |> Seq.distinct

    printfn $"Part 2: {Seq.length antinodes}"


let map =
    File.ReadAllLines("input.txt")

let mapHeight = map.Length
let mapWidth = map[0].Length
 
let positionsPerFreq =
    Seq.allPairs { 0 .. mapHeight - 1 } { 0 .. mapWidth - 1 }
    |> Seq.filter (fun (r, c) -> map[r][c] <> '.')
    |> Seq.map (fun (r, c) -> { Y = r; X = c; })
    |> Seq.groupBy (fun p -> map[p.Y][p.X])

part1 mapHeight mapWidth positionsPerFreq
part2 mapHeight mapWidth positionsPerFreq
