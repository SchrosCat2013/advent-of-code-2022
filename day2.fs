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

let scoreRound2 round =
    (determineResult round |> scoreResult) + (scoreAction round.Me)

let parseOpponentAction = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> raise (Exception "Opponent choice was not one of A,B,C")

let parseMyAction = function
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> raise (Exception "My choice was not one of X,Y,Z")

let parseLineChallenge2 (line: string) =
    match line.Split " " with
    | [| opponent; me |] -> 
        {
            Opponent = parseOpponentAction opponent
            Me = parseMyAction me
        }
    | _ -> raise (Exception "Line did not contain two values")

[<Fact>]
let Challenge2 () =
    File.ReadLines("day2.txt")
    |> Seq.map parseLineChallenge2
    |> Seq.map scoreRound2
    |> Seq.sum
    |> should equal 11386

type Round1A = {
    Opponent: RockPaperScissors
    Outcome: Result
}

let determineMyAction opponent outcome =
    let actionProducesDesiredOutcome action =
        determineResult { Opponent = opponent; Me = action } = outcome
    
    [| Rock; Paper; Scissors |] |> Array.find actionProducesDesiredOutcome

let parseOutcome = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> raise (Exception "Outcome was not one of X,Y,Z")

let parseLineChallenge2A (line: string) =
    match line.Split " " with
    | [| opponent; outcome |] ->
        {
            Opponent = parseOpponentAction opponent
            Outcome = parseOutcome outcome
        }
    | _ -> raise (Exception "Line did not contain two values")

let scoreRound2A round2a =
    (scoreResult round2a.Outcome) + (determineMyAction round2a.Opponent round2a.Outcome |> scoreAction)

[<Fact>]
let Challenge2A () =
    File.ReadLines("day2.txt")
    |> Seq.map parseLineChallenge2A
    |> Seq.map scoreRound2A
    |> Seq.sum
    |> should equal 13600
