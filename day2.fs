module adventofcodeday2

open Xunit
open FsUnit.Xunit
open System
open System.IO

type RockPaperScissors = Rock | Paper | Scissors
type Result = Win | Lose | Draw

type Round = {
    Opponent: RockPaperScissors
    Me: RockPaperScissors
}

let determineResult = function
    | { Opponent = Rock; Me = Rock } -> Draw
    | { Opponent = Rock; Me = Paper } -> Win
    | { Opponent = Rock; Me = Scissors } -> Lose
    | { Opponent = Paper; Me = Rock } -> Lose
    | { Opponent = Paper; Me = Paper } -> Draw
    | { Opponent = Paper; Me = Scissors } -> Win
    | { Opponent = Scissors; Me = Rock } -> Win
    | { Opponent = Scissors; Me = Paper } -> Lose
    | { Opponent = Scissors; Me = Scissors } -> Draw

let scoreResult = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let scoreAction = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreRound round =
    (determineResult round |> scoreResult) + (scoreAction round.Me)

let parseLineChallenge2 (line: string) =
    let [| opponent; me |] = line.Split " "
    {
        Opponent =
            match opponent with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> raise (Exception "Opponent choice was not one of A,B,C")
        Me =
            match me with
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | _ -> raise (Exception "My choice was not one of X,Y,Z")
    }

[<Fact>]
let Challenge2 () =
    File.ReadLines("../../../day2.txt")
    |> Seq.map parseLineChallenge2
    |> Seq.map scoreRound
    |> Seq.sum
    |> should equal 11386

type Round1A = {
    Opponent: RockPaperScissors
    Outcome: Result
}

let determineAction opponent outcome =
    match (opponent, outcome) with
    | (Rock, Win) -> Paper
    | (Rock, Lose) -> Scissors
    | (Rock, Draw) -> Rock
    | (Paper, Win) -> Scissors
    | (Paper, Lose) -> Rock
    | (Paper, Draw) -> Paper
    | (Scissors, Win) -> Rock
    | (Scissors, Lose) -> Paper
    | (Scissors, Draw) -> Scissors

let parseLineChallenge2A (line: string) =
    let [| a; b |] = line.Split " "
    {
        Opponent =
            match a with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | _ -> raise (Exception "Opponent choice was not one of A,B,C")
        Outcome =
            match b with
            | "X" -> Lose
            | "Y" -> Draw
            | "Z" -> Win
            | _ -> raise (Exception "Outcome was not one of X,Y,Z")
    }

let convertRound round2a =
    {
        Opponent = round2a.Opponent
        Me = determineAction round2a.Opponent round2a.Outcome
    }

[<Fact>]
let Challenge2A () =
    File.ReadLines("../../../day2.txt")
    |> Seq.map parseLineChallenge2A
    |> Seq.map convertRound
    |> Seq.map scoreRound
    |> Seq.sum
    |> should equal 13600
