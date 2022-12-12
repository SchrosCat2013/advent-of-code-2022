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

type OperationToken =
    | Old
    | Plus
    | Multiply
    | Int of int

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
        Items = input[1] |> stringStartsWith "  Starting items: " |> splitString ", " |> Seq.map int |> List.ofSeq |> List.rev
        Operation = input[2] |> stringStartsWith "  Operation: new = " |> parseOperation
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
    // Lists in reverse order so we can add items to head of list easily
    sampleInput[0].Items |> should equal [98; 79]
    sampleInput[0].DivisibleTest |> should equal 23
    sampleInput[0].PassToIndexIfTrue |> should equal 2
    sampleInput[0].PassToIndexIfFalse |> should equal 3
    sampleInput[0].Operation 2 |> should equal 38

let nextMonkeyIndex monkey item =
    if item % monkey.DivisibleTest = 0
    then monkey.PassToIndexIfTrue
    else monkey.PassToIndexIfFalse

let takeMonkeyTurn (allMonkies: Monkey[]) (monkeyIndex: int) =
    let monkey = allMonkies[monkeyIndex]
    let items = allMonkies |> Array.map (fun m -> m.Items)

    let inspectItem (item: int) =
        let newWorryLevel = (monkey.Operation item) / 3
        let nextMonkey = nextMonkeyIndex monkey newWorryLevel
        items[nextMonkey] <- newWorryLevel::items[nextMonkey]

    items[monkeyIndex]
    |> List.rev
    |> List.iter inspectItem

    allMonkies
    |> Array.mapi (fun i m -> { m with Items = if i = monkeyIndex then [] else items[i] })

[<Fact>]
let Challenge11SampleAfterMonkey1TurnTest () =
    takeMonkeyTurn sampleInput 0
    |> Array.map (fun monkey -> monkey.Items)
    |> should equal [|
        []
        [74; 75; 65; 54]
        [97; 60; 79]
        [620; 500; 74]
    |]

let round (monkeys: Monkey[]) =
    (monkeys, { 0 .. monkeys.Length - 1 })
    ||> Seq.fold takeMonkeyTurn

[<Fact>]
let Challenge11SampleAfterRound1 () =
    sampleInput
    |> round
    |> Array.map (fun monkey -> monkey.Items)
    |> should equal [|
        [ 26; 27; 23; 20]
        [ 1046; 401; 207; 167; 25; 2080 ]
        []
        []
    |]

let addThrownItemCount (monkeys: Monkey[]) (currentCount: int[]) (index: int) =
    currentCount
    |> Array.updateAt index (currentCount[index] + monkeys[index].Items.Length)

let nTurns n (monkeys: Monkey[]) = seq {
        for _ in 1..n do
            yield! seq { 0..monkeys.Length-1 }
    }

let applyTupleFn (fx, fy) i = (fx i, fy i)

let countInspectionsOverNRounds (n: int) (monkeys: Monkey[]) =
    let folder (monkeys, count) =
        applyTupleFn (takeMonkeyTurn monkeys, addThrownItemCount monkeys count)

    let initialCount = Array.create monkeys.Length 0

    (20, monkeys)
    ||> nTurns
    |> Seq.fold folder (monkeys, initialCount)
    |> Operators.snd

[<Fact>]
let Challenge11SampleCountsAfter20Rounds () =
    sampleInput
    |> countInspectionsOverNRounds 20
    |> should equal [| 101; 95; 7; 105 |]
