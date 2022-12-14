module AdventOfCodeDay08

open Xunit
open FsUnit.Xunit
open System
open System.IO
open Utils

let stringToIntArray =
    Seq.map (string >> int)
    >> Array.ofSeq

let parseGrid =
    Seq.map stringToIntArray
    >> Array.ofSeq
    >> array2D

let sampleInput =
    seq {
        "30373"
        "25512"
        "65332"
        "33549"
        "35390"
    } |> parseGrid

let challengeInput =
    File.ReadLines "day08.txt"
    |> parseGrid

let asRows (array: 'T[,]) =
    let length = (Array2D.length1 array) - 1
    [| for i in 0..length -> array[i,*] |]

let asColumns (array: 'T[,]) =
    let width = (Array2D.length2 array) - 1
    [| for i in 0..width -> array[*,i] |]

let treeIsVisible (maxHeight: int) (treeHeight: int) =
    (treeHeight > maxHeight, max maxHeight treeHeight)

let treeIsVisibleBack a b = treeIsVisible b a

let determineRowOrColumnVisibilities (trees: int[]) =
    let (forwards, _) =
        (-1, trees)
        ||> Array.mapFold treeIsVisible

    let (reverse, _) =
        (trees, -1)
        ||> Array.mapFoldBack treeIsVisibleBack

    (forwards, reverse)
    ||> Array.map2 (||)

let determineAllVisibilities (grid: int[,]) =
    let rowVisibilities = grid |> asRows |> Array.map determineRowOrColumnVisibilities
    let columnVisibilities = grid |> asColumns |> Array.map determineRowOrColumnVisibilities

    let length = Array2D.length1 grid
    let width = Array2D.length2 grid
    let isVisible y x = rowVisibilities[y][x] || columnVisibilities[x][y]

    Array2D.init length width isVisible

let flatten (x: 'T[,]) = Seq.cast<'T> x

let countAllVisibleTrees =
    determineAllVisibilities
    >> flatten
    >> Seq.filter Operators.id
    >> Seq.length

[<Fact>]
let Challenge8Sample () =
    sampleInput
    |> countAllVisibleTrees
    |> should equal 21

[<Fact>]
let Challenge8 () =
    challengeInput
    |> countAllVisibleTrees
    |> should equal 1713

let rec countTreesVisibleFromLocationAtHeightInDirection grid location height count direction =
    let (x, y) = location |> move direction
    
    if (x, y) |> isOutOfBounds grid then count
    elif grid[y, x] >= height then count + 1
    else countTreesVisibleFromLocationAtHeightInDirection grid (x, y) height (count + 1) direction

let calculateScenicScore (grid: int[,]) (y: int) (x: int) (height: int) =
    [| Up; Down; Left; Right |]
    |> Seq.map (countTreesVisibleFromLocationAtHeightInDirection grid (x, y) height 0)
    |> Seq.reduce (*)

let calculateAllScenicScores grid =
    grid |> Array2D.mapi (calculateScenicScore grid)

[<Fact>]
let Challenge8ASample () =
    let scores = calculateAllScenicScores sampleInput
    scores[1,2] |> should equal 4
    scores[3,2] |> should equal 8
    
    scores
    |> flatten
    |> Seq.max
    |> should equal 8

[<Fact>]
let Challenge8A () =
    challengeInput
    |> calculateAllScenicScores
    |> flatten
    |> Seq.max
    |> should equal 268464
