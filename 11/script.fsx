open System

module IO =
    open System.IO
    let getAllLines fileName = Path.Join(__SOURCE_DIRECTORY__, fileName) |> File.ReadAllLines

module Testing =
    /// Passes the given `file` to `f` and validates that `f` yields `expected`, otherwise throws.
    let assertOutput f file expected =
        let actual = f file
        if actual <> expected then
            failwithf "Assertion failed! Expected %i, got %i" expected actual

[<RequireQualifiedAccess>]
type Operation =
    | MultiplyOldBy of int64
    | IncrementOldBy of int64
    | SquareOld
with
    static member apply old = function
        | MultiplyOldBy x -> old * x
        | IncrementOldBy x -> old + x
        | SquareOld -> old * old

type PassRules =
    { divisibleBy: int
      ifTruePassTo: int
      ifFalsePassTo: int }

type Monkey (
    monkeyId: int,
    initialItems: int seq,
    operation: Operation,
    passRules: PassRules,
    broker: Broker) =

    let items = ResizeArray(initialItems |> Seq.map (int64))
    let mutable itemInspectionsPerformed = 0L

    member _.Id = monkeyId
    member _.InspectionsPerformed = itemInspectionsPerformed

    member _.ReceiveItem (item: int64) =
        items.Add(item)

    member __.SimulateRound(manageAnxiety: bool) =
        items
        |> Seq.iter (fun item ->
            let newWorry =
                let x = Operation.apply item operation
                if manageAnxiety then
                    x / 3L
                else
                    x
            itemInspectionsPerformed <- itemInspectionsPerformed + 1L

            let target =
                if newWorry % int64(passRules.divisibleBy) = 0 then
                    passRules.ifTruePassTo
                else
                    passRules.ifFalsePassTo
            do broker.SendItemToMonkey(target, newWorry)
        )
        items.Clear()

    static member ofStrings b = function
        | [| (num: string); items; op; test; ifTrue; ifFalse |] ->
            let parseCharAtAsInt (s: string) c = s.Chars c |> string |> Int32.Parse

            let number = parseCharAtAsInt num 7

            let initialItems =
                items.Remove(0, 18).Split(',', StringSplitOptions.TrimEntries)
                |> Array.map Int32.Parse

            let operation =
                match op.Remove(0, 23).Split(' ') with
                | [| "+"; x |] -> Operation.IncrementOldBy (Int32.Parse x)
                | [| "*"; "old" |] -> Operation.SquareOld
                | [| "*"; x |] -> Operation.MultiplyOldBy (Int32.Parse x)
                | unknown -> failwithf "Could not parse operation for monkey %s: %A" num unknown

            let passRules =
                { divisibleBy = test.Remove(0, 21) |> Int32.Parse
                  ifTruePassTo = parseCharAtAsInt ifTrue 29
                  ifFalsePassTo = parseCharAtAsInt ifFalse 30 }

            Monkey(number, initialItems, operation, passRules, b)
        | (other: string[]) ->
            failwithf "Could not parse monkey: %s" (String.Join(Environment.NewLine, other))
and Broker() =
    let mutable monkeys = Map.empty

    member _.Add (ms: Monkey seq) =
        for m in ms do
            monkeys <- Map.add (m.Id) m monkeys

    member _.SendItemToMonkey (monkeyId: int, item: int64) =
        let dest = Map.find monkeyId monkeys
        dest.ReceiveItem(item)

    member private _.SimulateRound(manageAnxiety) =
        monkeys
        |> Seq.sortBy (fun (KeyValue(id, _)) -> id)
        |> Seq.iter (fun (KeyValue(_, m)) -> m.SimulateRound(manageAnxiety))

    member x.ScoreAfterRounds manageAnxiety (rounds: int) =
        for _ in 1..rounds do
            x.SimulateRound(manageAnxiety)

        monkeys
        |> Seq.map (fun (KeyValue(_, m)) ->
            printfn "M: %i - Ops: %i" m.Id m.InspectionsPerformed
            m.InspectionsPerformed
        )
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.reduce ( * )

let computeMonkeyBusinessAfterRounds manageAnxiety rounds file =
    let b = Broker()

    let assembleSimulation =
        IO.getAllLines
        >> Array.filter (String.IsNullOrWhiteSpace >> not)
        >> Array.chunkBySize 6
        >> Array.map (Monkey.ofStrings b)
        >> b.Add

    do assembleSimulation file
    b.ScoreAfterRounds manageAnxiety rounds

let testPartOne = Testing.assertOutput (computeMonkeyBusinessAfterRounds true 20)
testPartOne "test.txt" 10605
testPartOne "in.txt" 110888

// let testPartTwo = Testing.assertOutput (computeMonkeyBusinessAfterRounds false 1000)
// testPartTwo "test.txt" 2713310158L

