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

let getPositionsBeforeExiting (mapHeight: int) (mapWidth: int) (obstructions: Pos Set) (initialPerson: Person) =
    let mutable person = initialPerson
    let mutable visited: (Pos*Dir) Set = Set.empty
    let isVisited = fun(pers: Person) -> Set.contains (pers.Pos, pers.Dir) visited
    let isObstruction = fun(p) -> Set.contains p obstructions

    while person.Pos.Within(mapHeight, mapWidth) && not (isVisited person) do
        visited <- Set.add (person.Pos, person.Dir) visited
        if isObstruction (person.NextPos()) then
            person <- person.Turn()
        else
            person <- person.Move()

    let exited = not (person.Pos.Within(mapHeight, mapWidth))
    if exited then
        Some (Seq.map fst visited |> Seq.distinct |> List.ofSeq)
    else
        None


let part1 (mapHeight: int) (mapWidth: int) (obstructions: Pos Set) (initialPerson: Person) =
    let visited = getPositionsBeforeExiting mapHeight mapWidth obstructions initialPerson
    printfn $"Part 1: {Seq.length visited.Value}"

let part2 (mapHeight: int) (mapWidth: int) (obstructions: Pos Set) (initialPerson: Person) =
    let defaultPositions = (getPositionsBeforeExiting mapHeight mapWidth obstructions initialPerson).Value
    
    let loopMaps =
        defaultPositions
        |> Seq.filter (fun p -> p <> initialPerson.Pos)
        |> Seq.map (fun p -> Set.add p obstructions)
        |> Seq.filter (fun obs -> (getPositionsBeforeExiting mapHeight mapWidth obs initialPerson).IsNone)
        |> Seq.length

    printfn $"Part 2: {loopMaps}"

let map =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun str -> List.ofSeq str)
    |> List.ofSeq

let mapHeight = map.Length
let mapWidth = map[0].Length

let obstructions =
    Seq.allPairs { 0 .. map.Length - 1 } { 0 .. map[0].Length - 1 }
    |> Seq.filter (fun (r, c) -> map[r][c] = '#')
    |> Seq.map (fun (r, c) -> { R = r; C = c })
    |> Set.ofSeq

let pos =
    Seq.allPairs { 0 .. map.Length - 1 } { 0 .. map[0].Length - 1 }
    |> Seq.find (fun (r, c) -> Seq.contains (map[r][c])  ['^'; 'v'; '<'; '>'])
    |> fun (r, c) -> { R = r; C = c; }

let dir =
    match map[pos.R][pos.C] with
    | '^' -> Dir.Up
    | '>' -> Dir.Right
    | 'v' -> Dir.Down
    | '<' -> Dir.Left
    | _ -> failwith $"Unexpected start character {map[pos.R][pos.C]}"

let person = { Pos = pos; Dir = dir; }

part1 mapHeight mapWidth obstructions person
part2 mapHeight mapWidth obstructions person
