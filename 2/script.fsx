open System

module IO =
    open System.IO

    let getAllLines fileName =
        Path.Join(__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllLines

module Testing =
    /// Passes the given `file` to `f` and validates that `f` yields `expected`, otherwise throws.
    let assertOutput f file expected =
        let actual = f file
        if actual <> expected then
            failwithf "Assertion failed! Expected %i, got %i" expected actual

type Shape =
    | Rock
    | Paper
    | Scissors
with
    static member ofChar = function
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors
        | x -> failwithf "Expected chars A-C or X-Z, got %c" x

    member x.Score =
        match x with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type Round =
    { theirs: Shape
      ours: Shape }
with
    static member ofString (s: String): Round =
        { Round.theirs = Shape.ofChar (s.Chars 0)
          ours = Shape.ofChar (s.Chars 2) }

type Outcome =
    | Win
    | Loss
    | Draw
with
    member x.Score =
        match x with
        | Win -> 6
        | Loss -> 0
        | Draw -> 3

    static member ofChar = function
        | 'X' -> Loss
        | 'Y' -> Draw
        | 'Z' -> Win
        | x -> failwithf "Expected chars X-Z, got %c" x

    static member ofRound = function
        | { theirs=Scissors; ours=Rock } -> Win
        | { theirs=Paper; ours=Scissors } -> Win
        | { theirs=Rock; ours=Paper } -> Win
        | { theirs=theirs; ours=ours } when theirs = ours -> Draw
        | _ -> Loss

let scoreRound (r: Round): int =
    r.ours.Score + (Outcome.ofRound r).Score

// Part 1
let sumIdealScore =
    IO.getAllLines >> Seq.sumBy (Round.ofString >> scoreRound)

Testing.assertOutput sumIdealScore "test.txt" 15
Testing.assertOutput sumIdealScore "in.txt" 15632

// Part 2
type Strategy =
    { theirs: Shape
      outcome: Outcome }
with
    static member ofString (s: String): Strategy =
        { Strategy.theirs = Shape.ofChar (s.Chars 0)
          outcome = Outcome.ofChar (s.Chars 2) }

    member x.Ours =
        match x with
        | { theirs=theirs; outcome=Draw } -> theirs
        | { theirs=Scissors; outcome=Loss } -> Paper
        | { theirs=Rock; outcome=Loss } -> Scissors
        | { theirs=Paper; outcome=Loss } -> Rock
        | { theirs=Scissors; outcome=Win } -> Rock
        | { theirs=Rock; outcome=Win } -> Paper
        | { theirs=Paper; outcome=Win } -> Scissors

    static member toRound x =
        { Round.theirs = x.theirs; ours = x.Ours }

let sumPlannedScore =
    IO.getAllLines
    >> Seq.sumBy (Strategy.ofString >> Strategy.toRound >> scoreRound)

Testing.assertOutput sumPlannedScore "test.txt" 12
Testing.assertOutput sumPlannedScore "in.txt" 14416
