open System.IO

let part1 (blocks: int list) =
    // 00...111...22
    // Free space (the dots) is -1
    let diskMap =
        Seq.indexed blocks
        |> Seq.collect (fun (i, n) ->
            if i % 2 = 0 then
                let id = i / 2
                Seq.replicate n id
            else
                Seq.replicate n -1)
        |> List.ofSeq

    let numUsedBlocks =
        Seq.filter (fun id -> id >= 0) diskMap
        |> Seq.length

    let checksum =
        Seq.indexed (Seq.take numUsedBlocks diskMap)
        |> Seq.fold (fun (s: {| Checksum: int64; ReverseFileBlocks: int list |}) (i, id) ->
            if id = -1 then
                // Fill free space from the end
                let fillId = List.head s.ReverseFileBlocks
                {| s with
                    Checksum = s.Checksum + (int64 (i * fillId));
                    ReverseFileBlocks = List.tail s.ReverseFileBlocks |}
            else
                {| s with
                    Checksum = s.Checksum + (int64 (i * id)) |}
            )
            {|
                Checksum = 0L;
                ReverseFileBlocks = List.ofSeq (Seq.rev (Seq.filter (fun id -> id >= 0) diskMap))
            |}
        |> _.Checksum

    printfn $"Part 1: {checksum}"


let part2 (blocks: int list) =
    ()


// 23332
let blocks =
    File.ReadAllLines("input.txt")[0]
    |> Seq.map (fun c -> int c - int '0')
    |> List.ofSeq

part1 blocks
part2 blocks
