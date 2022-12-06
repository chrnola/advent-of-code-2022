open System
open System.Collections.Generic

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
            failwithf "Assertion failed! Expected %A, got %A" expected actual

type MoveCommand =
    { sourceStack: char
      destinationStack: char
      quantity: int }
with
    static member ofString (s: string) =
        match s.Split([|"move "; " from "; " to "|], StringSplitOptions.RemoveEmptyEntries) with
        | [| qty; src; dest |] ->
            { sourceStack = src.Chars 0; destinationStack = dest.Chars 0; quantity = int(qty) }
        | _ ->
            failwithf "Could not parse move command: %s" s

type ParserState =
    { crates: char[] list;
      moves: MoveCommand list }

let parse (xs: string[]) =
    Array.fold (fun (state, partTwo) next ->
        if String.IsNullOrWhiteSpace(next) then
            state, true
        else
            if partTwo then
                { state with moves = state.moves @ [MoveCommand.ofString next] }, partTwo
            else
                let x = next.ToCharArray() |> Array.chunkBySize 4 |> Array.map (fun x -> x.[1])
                { state with crates = x :: state.crates }, partTwo
    ) ({ ParserState.crates = []; moves = [] }, false) xs
    |> (fun (s, _) -> s)

type State = Map<char, Stack<char>>

let initState (ps: ParserState) =
    let rows = Array.ofList ps.crates
    let cols = rows.[0].Length - 1

    Map.ofSeq (seq {
        for c in 0..cols do
            let cell = rows.[0].[c]
            let s = Stack<char>()
            for r in 1..(rows.Length - 1) do
                let next = rows.[r].[c]
                if next <> ' ' then
                    s.Push(next)
            yield cell, s
    }), ps.moves

let rec computeState f (state: State, moves: MoveCommand list) =
    match moves with
    | [] -> state
    | { sourceStack = src; destinationStack = dest; quantity = qty } :: rest ->
        f (state.Item src) (state.Item dest) qty
        computeState f (state, rest)

let moveOne (source: Stack<char>) (dest: Stack<char>) qty =
    for _ in 1..qty do
        source.Pop() |> dest.Push

let peekTops (state: State): string =
    String (
        state
        |> Map.toArray
        |> Array.sortBy (fun (ch, _) -> ch)
        |> Array.map (fun (_, s) -> s.Peek())
    )

// Part 1
let findTopChars f =
    IO.getAllLines
    >> parse
    >> initState
    >> computeState f
    >> peekTops

Testing.assertOutput (findTopChars moveOne) "test.txt" "CMZ"
Testing.assertOutput (findTopChars moveOne) "in.txt" "GRTSWNJHH"

// Part 2
let moveMany (source: Stack<char>) (dest: Stack<char>) qty =
    seq {
        for _ in 1..qty do
            yield source.Pop()
    }
    |> Seq.rev
    |> Seq.iter (dest.Push)

Testing.assertOutput (findTopChars moveMany) "test.txt" "MCD"
Testing.assertOutput (findTopChars moveMany) "in.txt" "QLFQDBBHM"
