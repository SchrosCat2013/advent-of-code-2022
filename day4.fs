module adventofcodeday4

open Xunit
open FsUnit.Xunit
open System
open System.IO

type Range (start: int, end': int) =
    member this.Start = start
    member this.End = end'

    static member Parse(input: string) =
        match input.Split "-" with
        | [| start; end' |] -> Range(int start, int end')
        | _ -> raise (Exception "Range did not contain start and end values")

    member this.FullyContains (other: Range) =
        other.Start >= this.Start && other.End <= this.End

    member this.Overlaps (other: Range) =
        other.End >= this.Start && other.Start <= this.End

let parseLine (line:string) =
    match line.Split "," with
    | [| elf1; elf2 |] -> (Range.Parse elf1, Range.Parse elf2)
    | _ -> raise (Exception "Line did not contain two pairs")

let isEitherFullyContained (r1: Range, r2: Range) =
    r1.FullyContains(r2) || r2.FullyContains(r1)

[<Fact>]
let Challenge4 () =
    File.ReadLines("day4.txt")
    |> Seq.map parseLine
    |> Seq.filter isEitherFullyContained
    |> Seq.length
    |> should equal 466

[<Fact>]
let Challenge4a () =
    File.ReadLines("day4.txt")
    |> Seq.map parseLine
    |> Seq.filter (fun (elf1, elf2) -> elf1.Overlaps(elf2))
    |> Seq.length
    |> should equal 865
