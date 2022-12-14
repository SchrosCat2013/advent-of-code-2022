module AdventOfCodeDay09

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

[<Struct>]
type Move = {
    Direction: Direction
    Count: int
}

let parseDirection = function
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | _ -> raise (Exception "Failed to parse direction")

let parseLine (line: string) =
    match line.Split " " with
    | [| d; c |] -> {
            Direction = parseDirection d
            Count = int c
        }
    | _ -> raise (Exception "Failed to parse line")

let sampleInput =
    [|
        "R 4"
        "U 4"
        "L 3"
        "D 1"
        "R 4"
        "D 1"
        "L 5"
        "R 2"
    |] |> Array.map parseLine

let sampleInput2 =
    [|
        "R 5"
        "U 8"
        "L 8"
        "D 3"
        "R 17"
        "D 10"
        "L 25"
        "U 20"
    |] |> Array.map parseLine

let challengeInput =
    File.ReadLines "day09.txt"
    |> Seq.map parseLine
    |> Seq.cache

type Chain = (int * int)[]

let moveKnotInOneDimension (head: int) (tail: int) =
    tail + Math.Sign (head - tail)

let moveKnotTowardsHead (headX, headY) (knotX, knotY) =
    if abs (headX - knotX) <= 1 && abs (headY - knotY) <= 1
    then (knotX, knotY)
    else (moveKnotInOneDimension headX knotX, moveKnotInOneDimension headY knotY)

let moveKnotChain (chain: Chain) (direction: Direction) =
    let newHead = chain[0] |> move direction

    chain[1..]
    |> Array.scan moveKnotTowardsHead newHead

let allIntermediateChainStates (chain: Chain) (move: Move) =
    let allChainStates =
        Array.create move.Count move.Direction
        |> Array.scan moveKnotChain chain
    
    (allChainStates, Array.last allChainStates)

let InitialKnots length = Array.create length (0, 0)

let countDistinctTailPositionsForLength length input =
    (InitialKnots length, input)
    ||> Seq.mapFold allIntermediateChainStates
    |> Operators.fst
    |> Seq.concat
    |> Seq.map Array.last
    |> Seq.distinct
    |> Seq.length

type TestCase (name: string, input: Move seq, chainLength: int, expected: int) =
    member this.Name = name
    member this.Input = input
    member this.ChainLength = chainLength
    member this.Expected = expected

    override this.ToString () =
        sprintf "%s with ChainLength %d" this.Name this.ChainLength


type Tests () =
    static member TestCases
        with get () = seq {
            [| TestCase ("Sample Input", sampleInput, 2, 13) |]
            [| TestCase ("Sample Input 2", sampleInput2, 10, 36) |]
            [| TestCase ("Challenge Input", challengeInput, 2, 6337) |]
            [| TestCase ("Challenge Input", challengeInput, 10, 2455) |]
        }

    [<Fact>]
    member _.Challenge9SampleFinalPosition () =
        (InitialKnots 2, sampleInput)
        ||> Seq.mapFold allIntermediateChainStates
        |> Operators.snd
        |> should equal [| (2, 2); (1, 2) |]

    [<Theory>]
    [<MemberData("TestCases")>]
    member _.Challenge9 (testCase:TestCase) =
        testCase.Input
        |> countDistinctTailPositionsForLength testCase.ChainLength
        |> should equal testCase.Expected

    // Utility method for debugging change failures
    // [<Fact>]
    member _.DumpAllPositions () =
        let printChain = Seq.map (sprintf "%O") >> String.concat ", "
        let printSequence = Seq.map printChain >> String.concat "\n  " >> sprintf "[\n  %s\n]"

        let text =
            (InitialKnots 2, challengeInput)
            ||> Seq.mapFold allIntermediateChainStates
            |> Operators.fst
            |> Seq.map printSequence
        
        File.WriteAllLines ("output.txt", text)
