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
    // Lists in reverse order so we can add items to head of list easily
    sampleInput[0].Items |> should equal [79; 98]
    sampleInput[0].DivisibleTest |> should equal 23
    sampleInput[0].PassToIndexIfTrue |> should equal 2
    sampleInput[0].PassToIndexIfFalse |> should equal 3
    sampleInput[0].Operation 2 |> should equal 38

let takeMonkeyTurn (inspectItem: int -> 'T -> (int * 'T)) (items: 'T list[]) (monkeyIndex: int) =
    let result =
        items
        |> Array.mapi (fun index value -> if index = monkeyIndex then [] else value)

    let throwItem (toMonkey, item) =
        result[toMonkey] <- item::result[toMonkey]

    items[monkeyIndex]
    |> List.rev
    |> List.map (inspectItem monkeyIndex)
    |> List.iter throwItem

    result

let nextMonkeyIndex monkey item =
    if item % monkey.DivisibleTest = 0
    then monkey.PassToIndexIfTrue
    else monkey.PassToIndexIfFalse

let inspectItem (monkeys: Monkey[]) (monkeyIndex: int) (item:int) =
    let monkey = monkeys[monkeyIndex]
    let newWorryLevel = (monkey.Operation item) / 3
    let nextMonkey = nextMonkeyIndex monkey newWorryLevel
    (nextMonkey, newWorryLevel)

let takeMonkeyTurn' monkeys = takeMonkeyTurn (inspectItem monkeys)

let initializeItems =
    Array.map (fun monkey -> monkey.Items |> List.rev)

[<Fact>]
let Challenge11SampleAfterMonkey1TurnTest () =
    (initializeItems sampleInput, 0)
    ||> takeMonkeyTurn' sampleInput
    |> should equal [|
        []
        [74; 75; 65; 54]
        [97; 60; 79]
        [620; 500; 74]
    |]

[<Fact>]
let Challenge11SampleAfterRound1 () =
    (initializeItems sampleInput, [0..sampleInput.Length-1])
    ||> Seq.fold (takeMonkeyTurn' sampleInput)
    |> should equal [|
        [ 26; 27; 23; 20]
        [ 1046; 401; 207; 167; 25; 2080 ]
        []
        []
    |]

let addThrownItemCount (items: 'T list[]) (currentCount: int[]) (index: int) =
    currentCount
    |> Array.updateAt index (currentCount[index] + items[index].Length)

let nTurns n (items: 'T[]) = seq {
        for _ in 1..n do
            yield! seq { 0..items.Length-1 }
    }

let applyTupleFn (fx, fy) i = (fx i, fy i)

let countInspectionsOverNRounds takeTurn (n: int) (items: 'T list[]) =
    let folder (items, count) =
        applyTupleFn (takeTurn items, addThrownItemCount items count)

    let initialCount = Array.create items.Length 0

    nTurns n items
    |> Seq.fold folder (items, initialCount)
    |> Operators.snd

[<Fact>]
let Challenge11SampleCountsAfter20Rounds () =
    initializeItems sampleInput
    |> countInspectionsOverNRounds (takeMonkeyTurn' sampleInput) 20
    |> should equal [| 101; 95; 7; 105 |]

let monkeyBusinessLevel =
    Array.sortDescending
    >> Seq.take 2
    >> Seq.map int64
    >> Seq.reduce (*)

[<Fact>]
let Challenge11Sample () =
    initializeItems sampleInput
    |> countInspectionsOverNRounds (takeMonkeyTurn' sampleInput) 20
    |> monkeyBusinessLevel
    |> should equal 10605L

[<Fact>]
let Challenge11 () =
    initializeItems challengeInput
    |> countInspectionsOverNRounds (takeMonkeyTurn' challengeInput) 20
    |> monkeyBusinessLevel
    |> should equal 58056L


let inspectItem' (monkeys: Monkey[]) (monkeyIndex: int) (item:int[]) =
    let monkey = monkeys[monkeyIndex]
    let applyOp index value =
        (monkey.Operation value) % monkeys[index].DivisibleTest

    let newWorryLevels =
        item |> Array.mapi applyOp

    let nextMonkey = nextMonkeyIndex monkey newWorryLevels[monkeyIndex]
    (nextMonkey, newWorryLevels)

let initializeItems' (monkeys: Monkey[]) =
    let initializeItem item =
        monkeys
        |> Array.map (fun monkey -> item % monkey.DivisibleTest)

    monkeys
    |> Array.map (fun monkey -> monkey.Items |> List.map initializeItem |> List.rev)

let takeMonkeyTurn'' monkeys = takeMonkeyTurn (inspectItem' monkeys)

[<Fact>]
let Challenge11Part2SampleCountsAfter20Rounds () =
    initializeItems' sampleInput
    |> countInspectionsOverNRounds (takeMonkeyTurn'' sampleInput) 20
    |> should equal [| 99; 97; 8; 103 |]

[<Fact>]
let Challenge11Part2Sample () =
    initializeItems' sampleInput
    |> countInspectionsOverNRounds (takeMonkeyTurn'' sampleInput) 10000
    |> monkeyBusinessLevel
    |> should equal 2713310158L

[<Fact>]
let Challenge11Part2 () =
    initializeItems' challengeInput
    |> countInspectionsOverNRounds (takeMonkeyTurn'' challengeInput) 10000
    |> monkeyBusinessLevel
    |> should equal 15048718170L
