// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open AdventOfCode2016

open System
// Define your library scripting code here

let sInput = "R3, L2, L2, R4, L1, R2, R3, R4, L2, R4, L2, L5, L1, R5, R2, R2, L1, R4, R1, L5, L3, R4, R3, R1, L1, L5, L4, L2, R5, L3, L4, R3, R1, L3, R1, L3, R3, L4, R2, R5, L190, R2, L3, R47, R4, L3, R78, L1, R3, R190, R4, L3, R4, R2, R5, R3, R4, R3, L1, L4, R3, L4, R1, L4, L5, R3, L3, L4, R1, R2, L4, L3, R3, R3, L2, L5, R1, L4, L1, R5, L5, R1, R5, L4, R2, L2, R1, L5, L4, R4, R4, R3, R2, R3, L1, R4, R5, L2, L5, L4, L1, R4, L4, R4, L4, R1, R5, L1, R1, L5, R5, R1, R1, L3, L1, R4, L1, L4, L4, L3, R1, R4, R1, R1, R2, L5, L2, R4, L1, R3, L5, L2, R5, L4, R5, L5, R3, R4, L3, L3, L2, R2, L5, L5, R3, R4, R3, R4, R3, R1"

type Command = |Right of int | Left of int
type Direction = |N|E|S|W

let rec (>>|) (f:'a->'a) (n:int) =
    if n < 1 then
        failwith "you should choose at least for repeating once"
    else if n=1 then
        f
    else
        f >> (f>>|(n-1))

let directionToRight =
    function
    | N -> E
    | E -> S
    | S -> W
    | W -> N
let directionToLeft = directionToRight >>| 3

printfn "%A" (>>|)

type Position = {facing:Direction;x:int;y:int}
with
    static member (+) (p:Position, c:Command) = 
        let d, s= 
            match c with
            | Left(x) -> p.facing |> directionToLeft, x
            | Right(x) -> p.facing |> directionToRight, x
        match d with
        | N -> {p with facing = d; y = p.y+s}
        | E -> {p with facing = d; x = p.x+s}
        | S -> {p with facing = d; y = p.y-s}
        | W -> {p with facing = d; x = p.x-s}

    

let mapstringToCommand (s:string) : Command =
    let n = s.[1..] |> int
    match s.[0] with
    | 'R' -> Command.Right(int n)
    | 'L' -> Command.Left(int n)
    | _ -> failwith "format not supported"

let endPosition = sInput.Split([|','; ' '|]) |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map mapstringToCommand |> Array.fold (+) {facing=N;x=0;y=0}
let answer = (endPosition.x |> abs) + (endPosition.y |> abs)

