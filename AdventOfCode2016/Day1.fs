module Day1

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
let answerA = (endPosition.x |> abs) + (endPosition.y |> abs)

//b

let positionsBetween p1 p2 =
    if p1.x = p2.x then
        [for i in p1.y..(if p1.y<p2.y then 1 else -1)..p2.y -> {p1 with y = i}].[1..]
    else
        [for i in p1.x..(if p1.x<p2.x then 1 else -1)..p2.x -> {p1 with x = i}].[1..]

let rec findCross pos path commands =
    match commands with
    | [] -> failwith "answer not found"
    | command::commands' -> 
        let pos' = pos+command
        let positions = positionsBetween pos pos'
        if positions |> List.exists (fun p -> path |> List.exists (fun ep -> ep.x = p.x && ep.y = p.y)) then
            positions |> List.find (fun p -> path |> List.exists (fun ep -> ep.x = p.x && ep.y = p.y))
        else
            findCross pos' (positions@path) commands'

let endPositionB = sInput.Split([|','; ' '|]) |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map mapstringToCommand |> Array.toList |> findCross {facing=N;x=0;y=0} []
let answerB = (endPositionB.x |> abs) + (endPositionB.y |> abs)
