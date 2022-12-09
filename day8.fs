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

type Direction = Up | Down | Left | Right

let rec countTreesVisibleAtHeightFromPositionInDirection (grid: int[,]) (y: int) (x: int) (height: int) (direction: Direction) =
    match direction with
    | Up when y = 0 -> 0
    | Up when grid[y - 1, x] >= height -> 1
    | Up -> 1 + countTreesVisibleAtHeightFromPositionInDirection grid (y - 1) x height Up
    | Down when y = Array2D.length2 grid - 1 -> 0
    | Down when grid[y + 1, x] >= height -> 1
    | Down -> 1 + countTreesVisibleAtHeightFromPositionInDirection grid (y + 1) x height Down
    | Left when x = 0 -> 0
    | Left when grid[y, x - 1] >= height -> 1
    | Left -> 1 + countTreesVisibleAtHeightFromPositionInDirection grid y (x - 1) height Left
    | Right when x = Array2D.length1 grid - 1 -> 0
    | Right when grid[y, x + 1] >= height -> 1
    | Right -> 1 + countTreesVisibleAtHeightFromPositionInDirection grid y (x + 1) height Right

let calculateScenicScore (grid: int[,]) (y: int) (x: int) (height: int) =
    [| Up; Down; Left; Right |]
    |> Seq.map (countTreesVisibleAtHeightFromPositionInDirection grid y x height)
    |> Seq.reduce (*)

let calculateAllScenicScores (input: string seq) =
    let grid = parseGrid input
    grid |> Array2D.mapi (calculateScenicScore grid)

[<Fact>]
let Challenge8ASamplePart1 () =
    let grid = parseGrid sampleInput

    calculateScenicScore grid 1 2 grid[1,2]
    |> should equal 4

    calculateScenicScore grid 3 2 grid[3,2]
    |> should equal 8

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
    File.ReadLines "day8.txt"
    |> calculateAllScenicScores
    |> flatten
    |> Seq.max
    |> should equal 268464
