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

let moveTailInOneDimension head tail =
    if head = tail then tail
    elif head < tail then tail - 1
    else tail + 1

let tryMoveTailTowardsHead (headX, headY) (tailX, tailY) =
    if abs (headX - tailX) <= 1 && abs (headY - tailY) <= 1
    then (tailX, tailY)
    else (moveTailInOneDimension headX tailX, moveTailInOneDimension headY tailY)

let rec moveKnots (knots: Knots) (direction: Direction) (count: int) (result: Knots list) =
    if count = 0
    then knots::result
    else
        let newHead = knots.Head |> move direction
        let newTail =  knots.Tail |> tryMoveTailTowardsHead newHead
        moveKnots { Head = newHead; Tail = newTail } direction (count - 1) (knots::result)

let moveKnots' (knots: Knots) (move: Move) =
    let allMoves = moveKnots knots move.Direction move.Count []
    (allMoves, allMoves.Head)

let InitialKnots = {
    Head = (0, 0)
    Tail = (0, 0)
}

[<Fact>]
let Challenge9SampleFinalPosition () =
    let (_, finalPosition) =
        (InitialKnots, sampleInput)
        ||> Seq.mapFold moveKnots'
    
    finalPosition
    |> should equal { Head = (2, 2); Tail = (1, 2) }

let countDistinctTailPositions input =
    (InitialKnots, input)
    ||> Seq.mapFold moveKnots'
    |> Operators.fst
    |> Seq.concat
    |> Seq.map (fun knot -> knot.Tail)
    |> Seq.distinct
    |> Seq.length

[<Fact>]
let Challenge9SampleDistinctTailPositions () =
    sampleInput
    |> countDistinctTailPositions
    |> should equal 13

[<Fact>]
let Challenge9 () =
    challengeInput
    |> countDistinctTailPositions
    |> should equal 6337

