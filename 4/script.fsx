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

let parseAssignmentRange (s: string) =
    match s.Split('-') |> Array.map(Int32.TryParse) with
    | [| (true, lower); (true, upper) |] ->
        if lower > upper then
            failwithf "Invalid bounds! %i should not be greater than %i" lower upper
        else
            (lower, upper)
    | _ ->
        failwithf "Could not split assignment range on '-', got '%s'" s

let parseAssignmentPair (s: string) =
    match s.Split(',') with
    | [| first; second |] ->
        parseAssignmentRange first, parseAssignmentRange second
    | _ ->
        failwithf "Could not split assignment pair on ',', got '%s'" s

let countOverlaps f =
    IO.getAllLines
    >> Seq.filter (parseAssignmentPair >> f)
    >> Seq.length

// Part 1
let fullyCovers ((a1, a2), (b1, b2)) =
    (a1 <= b1 && a2 >= b2)
    ||
    (b1 <= a1 && b2 >= a2)

let testPartOne = Testing.assertOutput (countOverlaps fullyCovers)
testPartOne "test.txt" 2
testPartOne "in.txt" 530

// Part 2
let overlapsAtAll ((a1, a2), (b1, b2)) =
    (a2 >= b1 && a2 <= b2)
    ||
    (a1 >= b1 && a1 <= b2)
    ||
    (b1 >= a1 && b1 <= a2)
    ||
    (b2 >= a1 && b2 <= a2)

let testPartTwo = Testing.assertOutput (countOverlaps overlapsAtAll)
testPartTwo "test.txt" 4
testPartTwo "in.txt" 903
