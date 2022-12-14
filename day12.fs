module AdventOfCodeDay12

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

type Input = {
    Start: (int * int)
    End: (int * int)
    GridHeights: int[,]
}

let parseChar (y: int) (x: int) = function
    | 'S' -> (Some (x, y), None, 0)
    | 'E' -> (None, Some (x, y), 25)
    | h when h >= 'a' && h <= 'z' -> (None, None, int h - int 'a')
    | _ -> raise (Exception "Could not parse height")

let pickStart (s, _, _) = s
let pickEnd (_, e, _) = e
let pickGrid (_, _, g) = g

let parseRow (y: int) (str: string) =
    let row = str |> Seq.mapi (parseChar y)
    (
        row |> Seq.tryPick pickStart,
        row |> Seq.tryPick pickEnd,
        row |> Seq.map pickGrid |> Array.ofSeq
    )

let parseInput (input: string seq) =
    let grid =
        input
        |> Seq.mapi parseRow
        |> Array.ofSeq
    
    {
        Start = grid |> Array.pick pickStart
        End = grid |> Array.pick pickEnd
        GridHeights = grid |> Array.map pickGrid |> array2D
    }

let sampleInput =
    [|
        "Sabqponm"
        "abcryxxl"
        "accszExk"
        "acctuvwj"
        "abdefghi"
    |] |> parseInput

let challengeInput =
    File.ReadLines "day12.txt"
    |> parseInput

[<Fact>]
let CanParseSampleInput () =
    sampleInput
    |> should equal {
        Start = (0, 0)
        End = (5, 2)
        GridHeights = array2D [|
            [| 0; 0; 1; 16; 15; 14; 13; 12 |]
            [| 0; 1; 2; 17; 24; 23; 23; 11 |]
            [| 0; 2; 2; 18; 25; 25; 23; 10 |]
            [| 0; 2; 2; 19; 20; 21; 22;  9 |]
            [| 0; 1; 3;  4;  5;  6;  7;  8 |]
        |]
    }
