open System.IO

let compareSequence = Seq.compareWith Operators.compare

let isSequenceXMAS (input: char seq) =
    compareSequence input (seq "XMAS") = 0

let matchXMASRight (lines: string list) y x =
    let cols = lines[0].Length
    x <= cols - 4 && lines[y][x..x+3] = "XMAS"
    
let matchXMASLeft (lines: string list) y x =
    x >= 3 && lines[y][x-3..x] = "SAMX"

let matchXMASDown (lines: string list) y x =
    let rows = lines.Length
    let chars = Seq.map (fun y2 -> lines[y2][x]) { y .. y+3 }
    y <= rows - 4 && isSequenceXMAS chars

let matchXMASUp (lines: string list) y x =
    let chars = Seq.map (fun y2 -> lines[y2][x]) { y .. -1 .. y-3 }
    y >= 3 && isSequenceXMAS chars

let matchXMASDownRight (lines: string list) y x =
    let rows = lines.Length
    let cols = lines[0].Length
    let indices = Seq.zip { y .. y+3 } { x .. x+3 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y <= rows - 4 && x <= cols - 4 && isSequenceXMAS chars

let matchXMASDownLeft (lines: string list) y x =
    let rows = lines.Length
    let indices = Seq.zip { y .. y+3 } { x .. -1 .. x-3 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y <= rows - 4 && x >= 3 && isSequenceXMAS chars

let matchXMASUpRight (lines: string list) y x =
    let cols = lines[0].Length
    let indices = Seq.zip { y .. -1 .. y-3 } { x .. x+3 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 3 && x <= cols - 4 && isSequenceXMAS chars

let matchXMASUpLeft (lines: string list) y x =
    let indices = Seq.zip { y .. -1 .. y-3 } { x .. -1 .. x-3 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 3 && x >= 3 && isSequenceXMAS chars

let xmasMatchers = [
    matchXMASRight; matchXMASLeft; matchXMASUp; matchXMASDown;
    matchXMASDownRight; matchXMASDownLeft; matchXMASUpRight; matchXMASUpLeft
]

let countXMASesFromPoint (lines: string List) y x =
    Seq.filter (fun m -> m lines y x) xmasMatchers
    |> Seq.length

let isSequenceMAS (input: char seq) =
    compareSequence input (seq "MAS") = 0

let matchMASRight (lines: string list) y x =
    let cols = lines[0].Length
    x >= 1 && x <= cols - 2 && lines[y][x-1..x+1] = "MAS"
    
let matchMASLeft (lines: string list) y x =
    let cols = lines[0].Length
    x >= 1 && x <= cols - 2 && lines[y][x-1..x+1] = "SAM"

let matchMASDown (lines: string list) y x =
    let rows = lines.Length
    let chars = Seq.map (fun y2 -> lines[y2][x]) { y-1 .. y+1 }
    y >= 1 && y <= rows - 2 && isSequenceMAS chars

let matchMASUp (lines: string list) y x =
    let rows = lines.Length
    let chars = Seq.map (fun y2 -> lines[y2][x]) { y+1 .. -1 .. y-1 }
    y >= 1 && y <= rows - 2 && isSequenceMAS chars

let matchMASDownRight (lines: string list) y x =
    let rows = lines.Length
    let cols = lines[0].Length
    let indices = Seq.zip { y-1 .. y+1 } { x-1 .. x+1 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 1 && y <= rows - 2 && x >= 1 && x <= cols - 2 && isSequenceMAS chars

let matchMASDownLeft (lines: string list) y x =
    let rows = lines.Length
    let cols = lines[0].Length
    let indices = Seq.zip { y-1 .. y+1 } { x+1 .. -1 .. x-1 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 1 && y <= rows - 2 && x >= 1 && x <= cols - 2 && isSequenceMAS chars

let matchMASUpRight (lines: string list) y x =
    let rows = lines.Length
    let cols = lines[0].Length
    let indices = Seq.zip { y+1 .. -1 .. y-1 } { x-1 .. x+1 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 1 && y <= rows - 2 && x >= 1 && x <= cols - 2 && isSequenceMAS chars

let matchMASUpLeft (lines: string list) y x =
    let rows = lines.Length
    let cols = lines[0].Length
    let indices = Seq.zip { y+1 .. -1 .. y-1 } { x+1 .. -1 .. x-1 }
    let chars = Seq.map (fun (y2, x2) -> lines[y2][x2]) indices
    y >= 1 && y <= rows - 2 && x >= 1 && x <= cols - 2 && isSequenceMAS chars

let diagonalMASMatchers1 = [matchMASDownRight; matchMASUpLeft]
let diagonalMASMatchers2 = [matchMASDownLeft; matchMASUpRight]
let matchDiagonalMAS (lines: string list) y x =
    Seq.exists (fun (m1, m2) -> (m1 lines y x) && (m2 lines y x)) (Seq.allPairs diagonalMASMatchers1 diagonalMASMatchers2)

let part1 (lines: string List) =
    let rowIndices = { 0 .. lines.Length - 1 }
    let colIndices = { 0 .. lines[0].Length - 1 }
    let xIndices =
        Seq.allPairs rowIndices colIndices
        |> Seq.filter (fun (y, x) -> lines[y][x] = 'X')

    let xmases =
        xIndices
        |> Seq.sumBy (fun (y, x) -> countXMASesFromPoint lines y x)

    printfn $"Part 1: {xmases}"

let part2 (lines: string List) =
    let rowIndices = { 0 .. lines.Length - 1 }
    let colIndices = { 0 .. lines[0].Length - 1 }
    let aIndices =
        Seq.allPairs rowIndices colIndices
        |> Seq.filter (fun (y, x) -> lines[y][x] = 'A')

    let mases =
        aIndices
        |> Seq.filter (fun (y, x) -> matchDiagonalMAS lines y x)
        |> Seq.length
    
    printfn $"Part 2: {mases}"


// Entry point
let lines = List.ofArray (File.ReadAllLines("input.txt"))
part1 lines
part2 lines
