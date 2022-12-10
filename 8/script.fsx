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

// Builds a 2D int array from an array of parsed strings/lines
let buildArrays =
    Array.map (Seq.toArray >> Array.map (string >> Int32.Parse))

/// Walks over all the trees from:
    /// - left-to-right
    /// - right-to-left
    /// - top-to-bottom
    /// - bottom-to-top
/// returning the corrdinates and height value as part of each walk.
/// Unless singlePass is true, then only performs a left-to-right traversal.
let walkTrees singlePass (xs: int[][]) =
    let rows = Array.length xs
    let cols = Array.length (Array.head xs)

    seq {
        for row in 0..(rows-1) do
            let rowWalk = seq {
                for col in 0..(cols - 1) do
                    yield (row, col), xs.[row].[col]
            }

            if singlePass then
                yield rowWalk
            else
                yield rowWalk
                yield (Seq.rev rowWalk)

        for col in 0..(cols - 1) do
            // top -> bottom
            let columnWalk = seq {
                for row in 0..(rows - 1) do
                    yield (row, col), xs.[row].[col]
            }

            if not singlePass then
                yield columnWalk
                yield (Seq.rev columnWalk)
    }

/// Finds the coordiantes of all visible trees
let findVisibleTrees =
    Seq.fold (fun visibleTrees nextPath ->
        nextPath
        |> Seq.fold (fun s (coords, value) ->
            match s with
            | None, vis ->
                // No trees before us, we must be visible
                Some value, Set.add coords vis
            | Some (max: int), vis ->
                let vis' =
                    if value > max then Set.add coords vis else vis
                Some (Math.Max(max, value)), vis'
        ) (None, visibleTrees)
        |> (fun (_, viz) -> viz)
    ) Set.empty

// Part 1
let countVisibleTrees =
    IO.getAllLines
    >> buildArrays
    >> walkTrees false
    >> findVisibleTrees
    >> Set.count

let testPartOne = Testing.assertOutput countVisibleTrees
testPartOne "test.txt" 21
testPartOne "in.txt" 1676

// Part 2
let calcScenicScore (xs: int[][]) ((row, col), height) =
    let rows = Array.length xs
    let cols = Array.length (Array.head xs)

    /// Counts the number of trees until one of equal/greater height is found (including that one) or the edge
    let rec scoreWalk num s =
        match Seq.tryHead s with
        | None -> num
        | Some h ->
            let num' = num + 1
            if h >= height then
                num'
            else
                scoreWalk num' (Seq.tail s)

    seq {
        // Walk right
        yield seq {
            for c in (col+1)..(cols-1) do
                yield xs.[row].[c]
        }

        // Left
        yield seq {
            for c in (col - 1)..(-1)..0 do
                yield xs.[row].[c]
        }

        // Down
        yield seq {
            for r in (row + 1)..(rows - 1) do
                yield xs.[r].[col]
        }

        // Up
        yield seq {
            for r in (row - 1)..(-1)..0 do
                yield xs.[r].[col]
        }
    }
    |> Seq.map (scoreWalk 0)
    |> Seq.reduce ( * )

let findMaxScenicScore file =
    let trees =
        IO.getAllLines file
        |> buildArrays

    walkTrees true trees
    |> Seq.concat
    |> Seq.map (calcScenicScore trees)
    |> Seq.max

let testPartTwo = Testing.assertOutput findMaxScenicScore
testPartTwo "test.txt" 8
testPartTwo "in.txt" 313200