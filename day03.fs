module AdventOfCodeDay03

open Xunit
open FsUnit.Xunit
open System
open System.IO

let splitCompartments (line:string) =
    let halfLength = line.Length / 2
    (line[..halfLength - 1], line[halfLength..])

let findCommonElement (compartment1:string, compartment2:string) =
    Set.ofSeq compartment1
    |> Set.intersect (Set.ofSeq compartment2)
    |> Seq.exactlyOne

let priorityOfItem = function
    | item when item >= 'a' && item <= 'z' ->
        (int item) - (int 'a') + 1
    | item when item >= 'A' && item <= 'Z' ->
        (int item) - (int 'A') + 27
    | _ -> raise (Exception "Item was not in [a..zA..Z]")

[<Fact>]
let Challenge3 () =
    File.ReadLines("day03.txt")
    |> Seq.map (splitCompartments >> findCommonElement >> priorityOfItem)
    |> Seq.sum
    |> should equal 7980

let findBadge = function
    | [| elf1; elf2; elf3 |] ->
        Set.ofSeq elf1
        |> Set.intersect (Set.ofSeq elf2)
        |> Set.intersect (Set.ofSeq elf3)
        |> Seq.exactlyOne
    | _ -> raise (Exception "Group did not contain three elfs")

[<Fact>]
let Challenge3a () =
    File.ReadLines("day03.txt")
    |> Seq.chunkBySize 3
    |> Seq.map (findBadge >> priorityOfItem)
    |> Seq.sum
    |> should equal 2881
