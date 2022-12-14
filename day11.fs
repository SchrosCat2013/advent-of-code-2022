module AdventOfCodeDay11

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

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

let inputValue = function
    | "old" -> (fun old -> old)
    | value -> (fun _ -> int value)

let infixOperatorOperation operator lhs rhs old =
    operator (old |> inputValue lhs) (old |> inputValue rhs)

let parseOperation (str: string) =
    match str.Split " " with
    | [| lhs; "+"; rhs |] -> infixOperatorOperation (+) lhs rhs
    | [| lhs; "*"; rhs |] -> infixOperatorOperation (*) lhs rhs
    | _ -> raise (Exception <| sprintf "Could not parse operation %s" str)

let parseMonkey (input: string[]) =
    {
        Items = input[1] |> stringStartsWith "  Starting items: " |> splitString ", " |> Seq.map int |> List.ofSeq
        Operation = input[2] |> stringStartsWith "  Operation: new = " |> parseOperation
        DivisibleTest = input[3] |> stringStartsWith "  Test: divisible by " |> int
        PassToIndexIfTrue = input[4] |> stringStartsWith "    If true: throw to monkey " |> int
        PassToIndexIfFalse = input[5] |> stringStartsWith "    If false: throw to monkey " |> int
    }

let readInput =
    File.ReadLines
    >> Seq.chunkBySize 7
    >> Seq.map parseMonkey
    >> Array.ofSeq

let sampleInput = readInput "day11-sample.txt"
let challengeInput = readInput "day11.txt"

[<Fact>]
let Challenge11SampleParseTest () =
    sampleInput[0].Items |> should equal [ 79; 98 ]
    sampleInput[0].DivisibleTest |> should equal 23
    sampleInput[0].PassToIndexIfTrue |> should equal 2
    sampleInput[0].PassToIndexIfFalse |> should equal 3
    sampleInput[0].Operation 2 |> should equal 38

let takeMonkeyTurn (inspectItem: int -> 'T -> (int * 'T)) (heldItems: 'T list[]) (monkeyIndex: int) =
    let thrownItems =
        heldItems[monkeyIndex]
        |> List.map (inspectItem monkeyIndex)

    let catchItems index items =
        if index = monkeyIndex
        then List.empty
        else
            thrownItems
            |> List.choose (fun (i, x) -> if i = index then Some x else None)
            |> List.append items

    heldItems
    |> Array.mapi catchItems

let nextMonkeyIndex monkey item =
    if item % monkey.DivisibleTest = 0
    then monkey.PassToIndexIfTrue
    else monkey.PassToIndexIfFalse

// Returns a tuple of the curried takeMonkeyTurn fn, and the initialized item array
let initializeForIntItemType (monkeys: Monkey[]) =
    let inspectItem (monkeyIndex: int) (item:int) =
        let monkey = monkeys[monkeyIndex]
        let newWorryLevel = (monkey.Operation item) / 3
        let nextMonkey = nextMonkeyIndex monkey newWorryLevel
        (nextMonkey, newWorryLevel)

    (
        takeMonkeyTurn inspectItem
        , monkeys |> Array.map (fun monkey -> monkey.Items)
    )

[<Fact>]
let Challenge11SampleAfterMonkey1TurnTest () =
    let (takeMonkeyTurn, items) = initializeForIntItemType sampleInput
    
    takeMonkeyTurn items 0
    |> should equal [|
        [ ]
        [ 54; 65; 75; 74 ]
        [ 79; 60; 97 ]
        [ 74; 500; 620 ]
    |]

[<Fact>]
let Challenge11SampleAfterRound1 () =
    let (takeMonkeyTurn, items) = initializeForIntItemType sampleInput

    (items, { 0..3 })
    ||> Seq.fold takeMonkeyTurn
    |> should equal [|
        [ 20; 23; 27; 26 ]
        [ 2080; 25; 167; 207; 401; 1046 ]
        [ ]
        [ ]
    |]

let addThrownItemCount (items: 'T list[]) (currentCount: int[]) (index: int) =
    currentCount
    |> Array.updateAt index (currentCount[index] + items[index].Length)

let nTurns n (items: 'T[]) = seq {
        for _ in 1..n do
            yield! seq { 0..items.Length-1 }
    }

let applyTupleFn (fx, fy) i = (fx i, fy i)

let countInspectionsOverNRounds (n: int) (takeTurn: 'T list[] -> int -> 'T list[]) (items: 'T list[]) =
    let folder (items, count) =
        applyTupleFn (takeTurn items, addThrownItemCount items count)

    let initialCount = Array.create items.Length 0

    nTurns n items
    |> Seq.fold folder (items, initialCount)
    |> Operators.snd

[<Fact>]
let Challenge11SampleCountsAfter20Rounds () =
    initializeForIntItemType sampleInput
    ||> countInspectionsOverNRounds 20
    |> should equal [| 101; 95; 7; 105 |]

let monkeyBusinessLevel =
    Array.sortDescending
    >> Seq.take 2
    >> Seq.map int64
    >> Seq.reduce (*)

[<Fact>]
let Challenge11Sample () =
    initializeForIntItemType sampleInput
    ||> countInspectionsOverNRounds 20
    |> monkeyBusinessLevel
    |> should equal 10605L

[<Fact>]
let Challenge11 () =
    initializeForIntItemType challengeInput
    ||> countInspectionsOverNRounds 20
    |> monkeyBusinessLevel
    |> should equal 58056L

let initializeForArrayItemType (monkeys: Monkey[]) =
    let inspectItem (monkeyIndex: int) (item:int[]) =
        let applyOp i value =
            (monkeys[monkeyIndex].Operation value) % monkeys[i].DivisibleTest

        let newWorryLevels =
            item |> Array.mapi applyOp

        let nextMonkey = nextMonkeyIndex monkeys[monkeyIndex] newWorryLevels[monkeyIndex]
        (nextMonkey, newWorryLevels)

    let initializeItem item =
        monkeys
        |> Array.map (fun monkey -> item % monkey.DivisibleTest)

    (
        takeMonkeyTurn inspectItem
        , monkeys |> Array.map (fun monkey -> monkey.Items |> List.map initializeItem)
    )

[<Fact>]
let Challenge11Part2SampleCountsAfter20Rounds () =
    initializeForArrayItemType sampleInput
    ||> countInspectionsOverNRounds 20
    |> should equal [| 99; 97; 8; 103 |]

[<Fact>]
let Challenge11Part2Sample () =
    initializeForArrayItemType sampleInput
    ||> countInspectionsOverNRounds 10000
    |> monkeyBusinessLevel
    |> should equal 2713310158L

[<Fact>]
let Challenge11Part2 () =
    initializeForArrayItemType challengeInput
    ||> countInspectionsOverNRounds 10000
    |> monkeyBusinessLevel
    |> should equal 15048718170L
