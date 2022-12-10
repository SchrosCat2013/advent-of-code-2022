module AdventOfCodeDay10

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

type Operation =
    | AddX of int
    | Noop

let parseLine = function
    | "noop" -> Noop
    | StartsWith "addx " x -> AddX (int x)
    | _ -> raise (Exception "Encountered unknown operation")

let parseFile filename =
    File.ReadLines filename
    |> Seq.map parseLine

[<Fact>]
let Challenge10Sample () =
    parseFile "day10-sample.txt"
    |> should be Empty
