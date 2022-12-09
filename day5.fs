module adventofcodeday5

open Xunit
open FsUnit.Xunit
open System
open System.IO
open System.Text.RegularExpressions

let parseCrateLayout =
    Seq.chunkBySize 4
    >> Seq.map (Seq.item 1)
    >> List.ofSeq

let transposeCrateColumns (parsedRows: char list list) =
    let accumulator (crate: char) (column: char list) =
        if crate = ' ' then column else crate::column

    let emptyColumnList = [for _ in parsedRows.Head -> []]

    (parsedRows, emptyColumnList)
    ||> List.foldBack (List.map2 accumulator)

let initialCrateLayout =
    File.ReadLines("day5a.txt")
    |> Seq.map parseCrateLayout
    |> List.ofSeq
    |> transposeCrateColumns

type MoveCratesAction = {
    SourceColumnIndex: int
    DestColumnIndex: int
    CrateCount: int
}

let parseAction (action: string) =
    let regexMatch = Regex.Match(action, @"move (\d+) from (\d+) to (\d+)")
    if regexMatch.Success
    then {
        CrateCount = int regexMatch.Groups.[1].Value
        SourceColumnIndex = (int regexMatch.Groups.[2].Value) - 1
        DestColumnIndex = (int regexMatch.Groups.[3].Value) - 1
    }
    else raise (Exception $"Move creates action did not parse - { action }")

let moveCratesActions =
    File.ReadLines("day5b.txt")
    |> Seq.map parseAction

let applyAction (crates: char list list) (action: MoveCratesAction) =
    let mutable sourceColumn = crates.[action.SourceColumnIndex]
    let mutable destColumn = crates.[action.DestColumnIndex]

    for _ in 1..action.CrateCount do
        destColumn <- sourceColumn.Head::destColumn
        sourceColumn <- sourceColumn.Tail

    crates
    |> List.updateAt action.SourceColumnIndex sourceColumn
    |> List.updateAt action.DestColumnIndex destColumn

[<Fact>]
let Challenge5 () =
    (initialCrateLayout, moveCratesActions)
    ||> Seq.fold applyAction
    |> Seq.map List.head
    |> (Array.ofSeq >> String)
    |> should equal "DHBJQJCCW"

let applyAction' (crates: char list list) (action: MoveCratesAction) =
    let mutable sourceColumn = crates.[action.SourceColumnIndex]
    let mutable destColumn = crates.[action.DestColumnIndex]

    destColumn <- sourceColumn[..action.CrateCount - 1] @ destColumn
    sourceColumn <- sourceColumn[action.CrateCount..]

    crates
    |> List.updateAt action.SourceColumnIndex sourceColumn
    |> List.updateAt action.DestColumnIndex destColumn

[<Fact>]
let Challenge5a () =
    (initialCrateLayout, moveCratesActions)
    ||> Seq.fold applyAction'
    |> Seq.map List.head
    |> (Array.ofSeq >> String)
    |> should equal "WJVRLSJJT"
