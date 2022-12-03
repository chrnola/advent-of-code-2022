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

let findRepeat (s: string): char =
    let mid = s.Length / 2
    let first = s.Substring(0, mid) |> set
    let second = s.Substring(mid)

    second.ToCharArray()
    |> Array.find (fun x -> Set.contains x first)

let getPriority (c: char) =
    let asciiCode = System.Convert.ToInt32(c)
    if asciiCode >= 97 then
        // lowercase
        asciiCode - 96
    else
        // capital
        (asciiCode - 38)

// Part 1
let calcPrioritySum =
    IO.getAllLines >> Seq.sumBy (findRepeat >> getPriority)

Testing.assertOutput calcPrioritySum "test.txt" 157
Testing.assertOutput calcPrioritySum "in.txt" 7674

// Part 2
let findGroupDuplicate (group: string[]) =
    group
    |> Array.map (set)
    |> Set.intersectMany
    |> Set.toList
    |> List.head

let calcGroupCommonPrioritySum =
    IO.getAllLines
    >> Seq.chunkBySize 3
    >> Seq.sumBy (findGroupDuplicate >> getPriority)

Testing.assertOutput calcGroupCommonPrioritySum "test.txt" 70
Testing.assertOutput calcGroupCommonPrioritySum "in.txt" 2805
