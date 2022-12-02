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

type RawManifest = seq<string>
type Calories = int

/// Stream decodes the given array into a sequence of summed calories.
/// (probably overkill given that we read the entire file into memory)
let parseManifest (xs: RawManifest): seq<Calories> = seq {
    if not (Seq.isEmpty xs) then
        let mutable sum = 0

        for x in xs do
            match System.Int32.TryParse(x) with
            | (true, num) ->
                sum <- sum + num
            | (false, _) ->
                yield sum
                sum <- 0

        yield sum
}

// Part 1
let getMax parser = IO.getAllLines >> parser >> Seq.max

let testPartOne = Testing.assertOutput (getMax parseManifest)
testPartOne "test.txt" 24000
testPartOne "in.txt" 68787

// Part 2
let getSumOfThreeLargest parser =
    IO.getAllLines
    >> parser
    >> Seq.sortDescending
    >> Seq.take 3
    >> Seq.sum

let testPartTwo = Testing.assertOutput (getSumOfThreeLargest parseManifest)
testPartTwo "test.txt" 45000
testPartTwo "in.txt" 198041

// Now for something more "functional"
[<RequireQualifiedAccess>]
type ManifestEntry =
    | Calories of int
    | Delimiter
with
    static member ofString (x: string) =
        match System.Int32.TryParse(x) with
        | (true, num) -> Calories num
        | (false, _) -> Delimiter

let obtuselyParseManifest (input: RawManifest): Calories list =
    Seq.fold (fun state next ->
        match state, ManifestEntry.ofString next with
        | [], ManifestEntry.Calories(c) -> c :: state
        | [], ManifestEntry.Delimiter -> failwith "First entry in the manifest shouldn't be a delimiter!"
        | x :: xs, ManifestEntry.Calories(c) -> (x + c) :: xs
        | x :: xs, ManifestEntry.Delimiter -> 0 :: x :: xs
    ) [] input

// Verify it still works
Testing.assertOutput (getMax obtuselyParseManifest)  "in.txt" 68787
Testing.assertOutput (getSumOfThreeLargest obtuselyParseManifest)  "in.txt" 198041
