module AdventOfCodeDay9

open Xunit
open FsUnit.Xunit
open System
open System.IO

type Direction =
    | Up | Down | Left | Right

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

let challengeInput =
    File.ReadLines "day9.txt"
    |> Seq.map parseLine

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

type Chain = (int * int)[]

let moveTailInOneDimension head tail =
    if head = tail then tail
    elif head < tail then tail - 1
    else tail + 1

let tryMoveChainTowardsHead (headX, headY) (tailX, tailY) =
    if abs (headX - tailX) <= 1 && abs (headY - tailY) <= 1
    then (tailX, tailY)
    else (moveTailInOneDimension headX tailX, moveTailInOneDimension headY tailY)

let moveKnotChain (chain: Chain) (direction: Direction) =
    let newHead = chain[0] |> move direction

    chain
    |> Array.tail
    |> Array.scan tryMoveChainTowardsHead newHead

let allIntermediateChainStates (chain: Chain) (move: Move) =
    let allChainStates =
        Array.create move.Count move.Direction
        |> Array.scan moveKnotChain chain
    
    (allChainStates, Array.last allChainStates)

let InitialKnots length = Array.create length (0, 0)

[<Fact>]
let Challenge9SampleFinalPosition () =
    (InitialKnots 2, sampleInput)
    ||> Seq.mapFold allIntermediateChainStates
    |> Operators.snd
    |> should equal [| (2, 2); (1, 2) |]

let countDistinctTailPositionsForLength length input =
    (InitialKnots length, input)
    ||> Seq.mapFold allIntermediateChainStates
    |> Operators.fst
    |> Seq.concat
    |> Seq.map Array.last
    |> Seq.distinct
    |> Seq.length

[<Fact>]
let Challenge9SampleDistinctTailPositions () =
    sampleInput
    |> countDistinctTailPositionsForLength 2
    |> should equal 13

[<Fact>]
let Challenge9 () =
    challengeInput
    |> countDistinctTailPositionsForLength 2
    |> should equal 6337

let challenge9AsampleInput =
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

[<Fact>]
let Challenge9ASample () =
    challenge9AsampleInput
    |> countDistinctTailPositionsForLength 10
    |> should equal 36

[<Fact>]
let Challenge9A () =
    challengeInput
    |> countDistinctTailPositionsForLength 10
    |> should equal 2455
