open System.IO

(*

Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0

*)

type OpCode =
    | Adv = 0
    | Bxl = 1
    | Bst = 2
    | Jnz = 3
    | Bxc = 4
    | Out = 5
    | Bdv = 6
    | Cdv = 7

type Reg =
    | A = 4
    | B = 5
    | C = 6

type Registers = int array

let getCombo (regs: Registers) (operand: int) =
    if operand < 4 then
        operand
    else
        regs[operand - 4]
    
let getReg (regs: Registers) (r: Reg) =
    regs[int r - 4]
    
let setReg (regs: Registers) (r: Reg) (v: int) =
    regs[int r - 4] <- v


let input = File.ReadAllLines("input.txt")

(*
    Register A: 729
    Register B: 0
    Register C: 0
*)
let regs =
    Seq.take 3 input
    |> Seq.map (fun ln -> int (ln.Split(": ")[1]))
    |> Array.ofSeq

let program =
    Seq.last input
    |> (fun ln ->
        // Program: 0,1,2,3
        let opsString = ln.Split(": ")[1]
        Seq.map int (opsString.Split(',')) |> Array.ofSeq)


printfn "Part 1:"

let mutable ip = 0
let mutable hasOutput = false
while ip < program.Length do
    let instr = enum<OpCode> program[ip]
    let op = program[ip + 1]

    match instr with
    | OpCode.Adv ->
        let combo = getCombo regs op
        let numerator = getReg regs Reg.A
        let denomiator = pown 2 combo
        let res = numerator / denomiator
        setReg regs Reg.A res
        ip <- ip + 2

    | OpCode.Bxl ->
        let lhs = getReg regs Reg.B
        let res = lhs ^^^ op
        setReg regs Reg.B res
        ip <- ip + 2

    | OpCode.Bst ->
        let combo = getCombo regs op
        let res = combo % 8
        setReg regs Reg.B res
        ip <- ip + 2

    | OpCode.Jnz ->
        let a = getReg regs Reg.A
        match a with
        | 0 -> ip <- ip + 2
        | _ -> ip <- op

    | OpCode.Bxc ->
        let b = getReg regs Reg.B
        let c = getReg regs Reg.C
        let res = b ^^^ c
        setReg regs Reg.B res
        ip <- ip + 2

    | OpCode.Out ->
        let combo = getCombo regs op
        let res = combo % 8
        if hasOutput then
            printf ","
        printf $"{res}"
        hasOutput <- true
        ip <- ip + 2

    | OpCode.Bdv ->
        let combo = getCombo regs op
        let numerator = getReg regs Reg.A
        let denomiator = pown 2 combo
        let res = numerator / denomiator
        setReg regs Reg.B res
        ip <- ip + 2

    | OpCode.Cdv ->
        let combo = getCombo regs op
        let numerator = getReg regs Reg.A
        let denomiator = pown 2 combo
        let res = numerator / denomiator
        setReg regs Reg.C res
        ip <- ip + 2

    | _ -> failwith $"Invalid instruction {program[ip]}"

