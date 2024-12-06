open System.IO

type Map = char list list


type Pos =
    { R: int; C: int; }

    member this.Within(height: int, width: int) =
        this.R >= 0 && this.R < height &&
        this.C >= 0 && this.C < width


type Dir =
    | Up
    | Right
    | Down
    | Left


type Person =
    { Pos: Pos; Dir: Dir }

    member this.NextPos() =
        match this.Dir with
        | Dir.Up ->    { R = this.Pos.R - 1 ; C = this.Pos.C }
        | Dir.Right -> { R = this.Pos.R     ; C = this.Pos.C + 1 }
        | Dir.Down ->  { R = this.Pos.R + 1 ; C = this.Pos.C }
        | Dir.Left ->  { R = this.Pos.R     ; C = this.Pos.C - 1 }

    member this.Turn() =
        let nextDir =
            match this.Dir with
            | Dir.Up -> Dir.Right
            | Dir.Right -> Dir.Down
            | Dir.Down -> Dir.Left
            | Dir.Left -> Dir.Up

        { Pos = this.Pos; Dir = nextDir }

    member this.Move() =
        { Pos = this.NextPos(); Dir = this.Dir }


let getVisitedPositions (map: Map) (pos: Pos) (dir: Dir) =
    let mutable person = { Pos = pos; Dir = dir }

    let mutable visitedPosDirs: (Pos * Dir) Set = Set.empty
    visitedPosDirs <- visitedPosDirs.Add((pos, dir))

    let mutable breakNow = false
    let mutable within = true
    let mutable looped = true

    while not breakNow do
        let nextPos = person.NextPos()
        if nextPos.Within(map.Length, map[0].Length) && map[nextPos.R][nextPos.C] = '#' then
            person <- person.Turn()
        else
            person <- person.Move()

        within <- person.Pos.Within(map.Length, map[0].Length)
        looped <- Set.contains (person.Pos, person.Dir) visitedPosDirs
        if not within || looped then
            breakNow <- true
        else
            visitedPosDirs <- visitedPosDirs.Add(person.Pos, person.Dir)

    let visitedPositions = visitedPosDirs |> Seq.map (fun pd -> fst pd) |> Seq.distinct |> List.ofSeq
    visitedPositions, looped


let addObstruction (map: Map) (pos: Pos) =
    (Seq.indexed map)
    |> Seq.map (fun (r, _) ->
        Seq.indexed map[r]
        |> Seq.map (fun (c, _) ->
            if r = pos.R && c = pos.C
            then '#'
            else map[r][c])
        |> List.ofSeq)
    |> List.ofSeq


let part1 (map: Map) (pos: Pos) (dir: Dir) =
    let visitedPositions, _ = getVisitedPositions map pos dir
    printfn $"Part 1: {Seq.length visitedPositions}"
    

let part2 (map: Map) (pos: Pos) (dir: Dir) =
    let defaultVisitedPositions = fst (getVisitedPositions map pos dir)

    let mapsMadeValid =
        defaultVisitedPositions
        |> Seq.filter (fun p -> p <> pos)
        |> Seq.map (fun p -> addObstruction map p)
        |> Seq.filter (fun m -> snd (getVisitedPositions m pos dir) = true)

    printfn $"Part 2: {Seq.length mapsMadeValid}"


let map =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun str -> List.ofSeq str)
    |> List.ofSeq

let pos =
    Seq.allPairs { 0 .. map.Length - 1 } { 0 .. map[0].Length - 1 }
    |> Seq.map (fun (r, c) -> { R = r; C = c })
    |> Seq.find (fun p -> Seq.contains (map[p.R][p.C])  ['^'; 'v'; '<'; '>'])

let dir =
    match map[pos.R][pos.C] with
    | '^' -> Dir.Up
    | '>' -> Dir.Right
    | 'v' -> Dir.Down
    | '<' -> Dir.Left
    | _ -> failwith $"Unexpected start character {map[pos.R][pos.C]}"


part1 map pos dir
part2 map pos dir
