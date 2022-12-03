﻿module adventofcodeday1

open Xunit
open FsUnit.Xunit
open System.IO

let challenge1Data () =
    seq {
        let numbers = File.ReadLines("../../../day1.txt")
        let mutable x = []

        for number in numbers do
            if number = "" then yield x
            x <- if number = "" then [] else (int number) :: x

        yield x
    }


[<Fact>]
let Challenge1 () =
    challenge1Data ()
    |> Seq.map List.sum
    |> Seq.max
    |> should equal 69626

[<Fact>]
let Challenge1A () =
    challenge1Data ()
    |> Seq.map List.sum
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> should equal 206780
