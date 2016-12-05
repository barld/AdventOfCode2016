module Day3

open System

#if INTERACTIVE
let path = __SOURCE_DIRECTORY__ + "/inputDay3.txt"
#else
let path = @"./inputDay3.txt"
#endif

let sInput = System.IO.File.ReadAllLines(path)

let isPossible a b c =
    a<b+c && b<a+c && c < a+b

let parse (s:string) =
    let res = s.Split(' ') |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map int
    isPossible res.[0] res.[1] res.[2]

let answer = sInput |> Array.map parse |> Array.filter id |> Array.length

//b
let parseB (s:string) =
    let res = s.Split(' ') |> Array.filter (String.IsNullOrWhiteSpace >> not) |> Array.map int
    res

let turn (arr:int[][]) =
    let arrLength = arr.Length
    let rec _loop row col =
        if row < arrLength && col<3 then
            isPossible arr.[row].[col] arr.[row+1].[col] arr.[row+2].[col]::_loop (row+3) col
        else if col<3 then
            _loop 0 (col+1)
        else
            []
    _loop 0 0
    

let answerB = sInput |> Array.map parseB |> turn |> List.filter id |> List.length
