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

[<Struct>]
type Knots = {
    Head: int * int
    Tail: int * int
}

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

type Chain = {
    Head: int * int
    Tail: (int * int)[]
}

let moveTailInOneDimension head tail =
    if head = tail then tail
    elif head < tail then tail - 1
    else tail + 1

let duplicateTuple x = (x,x)

let tryMoveChainTowardsHead (headX, headY) (tailX, tailY) =
    if abs (headX - tailX) <= 1 && abs (headY - tailY) <= 1
    then duplicateTuple (tailX, tailY)
    else duplicateTuple (moveTailInOneDimension headX tailX, moveTailInOneDimension headY tailY)

let rec moveKnotChain (chain: Chain) (direction: Direction) (count: int) (result: Chain list) =
    if count = 0
    then chain::result
    else
        let newHead = chain.Head |> move direction
        let (newTail, _) =
            (chain.Head, chain.Tail)
            ||> Array.mapFold tryMoveChainTowardsHead

        moveKnotChain { Head = newHead; Tail = newTail } direction (count - 1) (chain::result)

let moveKnotChain' (chain: Chain) (move: Move) =
    let allMoves = moveKnotChain chain move.Direction move.Count []
    (allMoves, allMoves.Head)


let InitialKnots length = {
    Head = (0, 0)
    Tail = Array.create (length - 1) (0, 0)
}

[<Fact>]
let Challenge9SampleFinalPosition () =
    let (_, finalPosition) =
        (InitialKnots 2, sampleInput)
        ||> Seq.mapFold moveKnotChain'
    
    finalPosition
    |> should equal { Head = (2, 2); Tail = [| (1, 2) |] }

let countDistinctTailPositionsForLength length input =
    (InitialKnots length, input)
    ||> Seq.mapFold moveKnotChain'
    |> Operators.fst
    |> Seq.concat
    |> Seq.map (fun chain -> chain.Tail |> Array.last)
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
