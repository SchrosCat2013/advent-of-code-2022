module adventofcodeday6

open Xunit
open FsUnit.Xunit
open System
open System.IO

let allUnique (items:'a seq) =
    items
    |> Seq.countBy Operators.id
    |> Seq.map Operators.snd
    |> Seq.max
    |> (=) 1

let findMarkerIndex (markerSize: int) (input: string) =
    input
    |> Seq.windowed markerSize
    |> Seq.findIndex allUnique
    |> (+) markerSize

[<Theory>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7)>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
let Challenge6sample (sample, expected) =
    sample
    |> findMarkerIndex 4
    |> should equal expected

[<Fact>]
let Challenge6 () =
    File.ReadAllText "day6.txt"
    |> findMarkerIndex 4
    |> should equal 1707

[<Theory>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19)>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 23)>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 23)>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29)>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)>]
let Challenge6asample (sample, expected) =
    sample
    |> findMarkerIndex 14
    |> should equal expected

[<Fact>]
let Challenge6a () =
    File.ReadAllText "day6.txt"
    |> findMarkerIndex 14
    |> should equal 3697
