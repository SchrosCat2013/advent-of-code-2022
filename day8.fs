module adventofcodeday8

open Xunit
open FsUnit.Xunit
open System
open System.IO

let sampleInput = [|
    "30373"
    "25512"
    "65332"
    "33549"
    "35390"
|]

let parseChar = string >> int

let parseGrid =
    Seq.map (Seq.map parseChar >> Array.ofSeq)
    >> Array.ofSeq
    >> array2D

let rows (array: 'T[,]) =
    let length = (Array2D.length1 array) - 1
    [| for i in 0..length -> array[i,*] |]

let columns (array: 'T[,]) =
    let width = (Array2D.length2 array) - 1
    [| for i in 0..width -> array[*,i] |]

let determineVisibilities (trees: int[]) =
    let treeIsVisible (maxHeight: int) (treeHeight: int) =
        (treeHeight > maxHeight, max maxHeight treeHeight)

    (-1, trees)
    ||> Array.mapFold treeIsVisible
    |> Operators.fst

let determineVisibilitiesBackwards (trees: int[]) =
    let treeIsVisible (treeHeight: int) (maxHeight: int) =
        (treeHeight > maxHeight, max maxHeight treeHeight)

    (trees, -1)
    ||> Array.mapFoldBack treeIsVisible
    |> Operators.fst

let determineAllVisibilities (grid: int[,]) =
    let rows = grid |> rows
    let columns = grid |> columns

    let leftToRight = rows |> Array.map determineVisibilities
    let rightToLeft = rows |> Array.map determineVisibilitiesBackwards
    let topToBottom = columns |> Array.map determineVisibilities
    let bottomToTop = columns |> Array.map determineVisibilitiesBackwards

    let length = Array2D.length1 grid
    let width = Array2D.length2 grid

    let isVisible y x =
        leftToRight[y][x]
        || rightToLeft[y][x]
        || topToBottom[x][y]
        || bottomToTop[x][y]

    Array2D.init length width isVisible

let flatten (x: 'T[,]) = Seq.cast<'T> x

let countAllVisibleTrees (input: string seq)=
    input
    |> parseGrid
    |> determineAllVisibilities
    |> flatten
    |> Seq.filter Operators.id
    |> Seq.length

[<Fact>]
let Challenge8Sample () =
    sampleInput
    |> countAllVisibleTrees
    |> should equal 21

[<Fact>]
let Challenge8 () =
    File.ReadLines "day8.txt"
    |> countAllVisibleTrees
    |> should equal 1713
