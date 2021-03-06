﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Scripts/load-project-debug.fsx"

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
    printfn "%A" res
    isPossible res.[0] res.[1] res.[2]

let answer = sInput |> Array.map parse |> Array.filter id |> Array.length

