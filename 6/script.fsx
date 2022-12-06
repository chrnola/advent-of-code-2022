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

/// Finds the last char index where a substring of length `markerSize`
/// appears containing all unique characters.
let findMarkerPosition markerSize =
    Seq.windowed markerSize
    >> Seq.findIndex (Set.ofSeq >> Set.count >> (=) markerSize)
    >> (+) markerSize

let sumMarkerPositions markerSize =
    IO.getAllLines
    >> Seq.sumBy(findMarkerPosition markerSize)

// Part 1
let testPartOne = Testing.assertOutput (sumMarkerPositions 4)
testPartOne "test.txt" 39
testPartOne "in.txt" 1235

// Part 2
let testPartTwo = Testing.assertOutput (sumMarkerPositions 14)
testPartTwo "test.txt" 120
testPartTwo "in.txt" 3051
