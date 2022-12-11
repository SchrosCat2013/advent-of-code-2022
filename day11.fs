module AdventOfCodeDay11

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils
open System.Text.RegularExpressions

type Monkey = {
    Items: int list
    Operation: (int -> int)
    DivisibleTest: int
    PassToIndexIfTrue: int
    PassToIndexIfFalse: int
}

let stringStartsWith (test: string) (str: string) =
    if str.StartsWith test
    then str[test.Length..]
    else raise (Exception <| sprintf "%s did not start with %s" str test)

let splitString (separator: string) (str: string) = str.Split separator

let parseMonkey (input: string[]) =
    {
        Items = input[1] |> stringStartsWith "  Starting items: " |> splitString ", " |> Seq.map int |> List.ofSeq
        Operation = Operators.id
        DivisibleTest = input[3] |> stringStartsWith "  Test: divisible by " |> int
        PassToIndexIfTrue = input[4] |> stringStartsWith "    If true: throw to monkey " |> int
        PassToIndexIfFalse = input[5] |> stringStartsWith "    If false: throw to monkey " |> int
    }

let sampleInput =
    File.ReadLines "day11-sample.txt"
    |> Seq.chunkBySize 7
    |> Seq.map parseMonkey
    |> Array.ofSeq

[<Fact>]
let Challenge11SampleParseTest () =
    sampleInput[0].Items |> should equal [79; 98]
    sampleInput[0].DivisibleTest |> should equal 23
    sampleInput[0].PassToIndexIfTrue |> should equal 2
    sampleInput[0].PassToIndexIfFalse |> should equal 3
    sampleInput[0].Operation 2 |> should equal 38
