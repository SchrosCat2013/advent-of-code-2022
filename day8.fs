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

let stringToIntArray =
    Seq.map (string >> int)
    >> Array.ofSeq

let parseGrid =
    Seq.map stringToIntArray
    >> Array.ofSeq
    >> array2D

let asRows (array: 'T[,]) =
    let length = (Array2D.length1 array) - 1
    [| for i in 0..length -> array[i,*] |]

let asColumns (array: 'T[,]) =
    let width = (Array2D.length2 array) - 1
    [| for i in 0..width -> array[*,i] |]

let treeIsVisible (maxHeight: int) (treeHeight: int) =
    (treeHeight > maxHeight, max maxHeight treeHeight)

let determineVisibilities (trees: int[]) =
    (-1, trees)
    ||> Array.mapFold treeIsVisible
    |> Operators.fst

let determineVisibilitiesBack (trees: int[]) =
    let treeIsVisible' a b = treeIsVisible b a

    (trees, -1)
    ||> Array.mapFoldBack treeIsVisible'
    |> Operators.fst

let determineAllVisibilities (grid: int[,]) =
    let rows = asRows grid
    let columns = asColumns grid

    let leftToRight = rows |> Array.map determineVisibilities
    let rightToLeft = rows |> Array.map determineVisibilitiesBack
    let topToBottom = columns |> Array.map determineVisibilities
    let bottomToTop = columns |> Array.map determineVisibilitiesBack

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

let move direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let isOutOfBounds grid (x, y) =
    (x < 0) || (x >= Array2D.length2 grid)
    || (y < 0) || (y >= Array2D.length1 grid)

let rec countTreesVisibleFromLocationAtHeightInDirection grid location height count direction =
    let (x, y) = location |> move direction
    
    if (x, y) |> isOutOfBounds grid then count
    elif grid[y, x] >= height then count + 1
    else countTreesVisibleFromLocationAtHeightInDirection grid (x, y) height (count + 1) direction

let calculateScenicScore (grid: int[,]) (y: int) (x: int) (height: int) =
    [| Up; Down; Left; Right |]
    |> Seq.map (countTreesVisibleFromLocationAtHeightInDirection grid (x, y) height 0)
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
