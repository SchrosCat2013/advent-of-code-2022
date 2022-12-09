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

let treeIsVisible (maxHeight: int) (treeHeight: int) =
    (treeHeight > maxHeight, max maxHeight treeHeight)

let determineVisibilities (trees: int[]) =
    (-1, trees)
    ||> Array.mapFold treeIsVisible
    |> Operators.fst

let determineAllVisibilities (grid: int[,]) =
    let rows = grid |> rows
    let columns = grid |> columns

    let leftToRight = rows |> Array.map determineVisibilities
    let rightToLeft = rows |> Array.map (Array.rev >> determineVisibilities)
    let topToBottom = columns |> Array.map determineVisibilities
    let bottomToTop = columns |> Array.map (Array.rev >> determineVisibilities)

    let length = Array2D.length1 grid
    let width = Array2D.length2 grid

    let isVisible x y =
        leftToRight[y][x]
        || rightToLeft[y][width - x - 1]
        || topToBottom[x][y]
        || bottomToTop[x][length - y - 1]

    Array2D.init width length isVisible

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
