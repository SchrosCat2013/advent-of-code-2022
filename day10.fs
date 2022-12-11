module AdventOfCodeDay10

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

type Operation =
    | AddX of int
    | Noop

let parseLine = function
    | "noop" -> Noop
    | StartsWith "addx " x -> AddX (int x)
    | _ -> raise (Exception "Encountered unknown operation")

let allStates =
    let accum x = function
    | Noop -> ([| x |], x)
    | AddX i -> ([| x; x |], x + i)
    
    Seq.mapFold accum 1
    >> Operators.fst
    >> Seq.concat
    >> Array.ofSeq

let parseFile =
    File.ReadLines
    >> Seq.map parseLine
    >> allStates

let sampleInput = parseFile "day10-sample.txt"
let challengeInput = parseFile "day10.txt"

let sumSignalStrength (states: int[]) =
    Seq.map (fun i -> (i + 1) * states[i])
    >> Seq.reduce (+)

[<Fact>]
let Challenge10Sample () =
    // The challenge clock cycles are 1-indexed, but our array is zero-indexed
    [| for i in 0..5 -> 19 + (40 * i) |]
    |> Array.map (Array.get sampleInput)
    |> should equalSeq [| 21; 19; 18; 21; 16; 18 |]

[<Fact>]
let Challenge10SampleSum () =
    [| for i in 0..5 -> 19 + (40 * i) |]
    |> sumSignalStrength sampleInput
    |> should equal 13140

[<Fact>]
let Challenge10 () =
    [| for i in 0..5 -> 19 + (40 * i) |]
    |> sumSignalStrength challengeInput
    |> should equal 11220

[<Fact>]
let SampleStateArrayLengthIs240 () =
    sampleInput
    |> Array.length
    |> should equal 240

[<Fact>]
let StateArrayLengthIs240 () =
    challengeInput
    |> Array.length
    |> should equal 240

let displayOutputForStateArray =
    let pixel x spritePosition =
        if (x >= spritePosition - 1) && (x <= spritePosition + 1)
        then '\u2588'
        else ' '

    Seq.chunkBySize 40
    >> Seq.map (Array.mapi pixel >> String)
    >> String.concat "\n"

[<Fact>]
let Challenge10ASample () =
    sampleInput
    |> displayOutputForStateArray
    |> should equal ("
██  ██  ██  ██  ██  ██  ██  ██  ██  ██  
███   ███   ███   ███   ███   ███   ███ 
████    ████    ████    ████    ████    
█████     █████     █████     █████     
██████      ██████      ██████      ████
███████       ███████       ███████     ".Trim())

[<Fact>]
let Challenge10A () =
    challengeInput
    |> displayOutputForStateArray
    |> should equal ("
███  ████ ███   ██    ██ ████ █    █  █ 
█  █    █ █  █ █  █    █ █    █    █ █  
███    █  █  █ █  █    █ ███  █    ██   
█  █  █   ███  ████    █ █    █    █ █  
█  █ █    █    █  █ █  █ █    █    █ █  
███  ████ █    █  █  ██  ████ ████ █  █ ".Trim())
