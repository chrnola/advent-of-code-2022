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
type Instruction =
    | AddX of int
    | NoOp
with
    static member ofString (s: string) =
        match s.Split(' ') with
        | [| "noop" |] -> Instruction.NoOp
        | [| "addx"; num |] -> Instruction.AddX (System.Int32.Parse num)
        | _ -> failwithf "Could not parse instruction: %s" s

type MachineState =
    { registerValue: int; cycleCount: int }
with
    member x.signalStrength =
        x.registerValue * x.cycleCount

    member x.SpritePixels =
        Set.ofList [ x.registerValue - 1; x.registerValue; x.registerValue + 1 ]

    member x.pixelState =
        let pixel = (x.cycleCount - 1) % 40
        Set.contains pixel x.SpritePixels

    member x.pixelValue =
        if x.pixelState then '#' else '.'

let computeStates (xs: Instruction seq) = seq {
    let mutable register = 1
    let mutable cycleCount = 1

    for instr in xs do
        match instr with
        | Instruction.AddX x ->
            yield { registerValue = register; cycleCount = cycleCount }
            cycleCount <- cycleCount + 1
            yield { registerValue = register; cycleCount = cycleCount }
            cycleCount <- cycleCount + 1
            register <- register + x
        | Instruction.NoOp ->
            yield { registerValue = register; cycleCount = cycleCount }
            cycleCount <- cycleCount + 1
}

// Interesting is defined as cycle 20 and then every 40 cycles after that
let getInterestingSignals =
    Seq.choose (fun state ->
        if state.cycleCount = 20 || (state.cycleCount - 20) % 40 = 0 then
            Some state
        else
            None
    )

let computeStatesFromFile =
    IO.getAllLines
    >> Seq.map Instruction.ofString
    >> computeStates

// Part 1
let sumInterestingSignalStrengths =
    computeStatesFromFile
    >> getInterestingSignals
    >> Seq.sumBy (fun state -> state.signalStrength)

let testPartOne = Testing.assertOutput sumInterestingSignalStrengths
testPartOne "test.txt" 13140
testPartOne "in.txt" 14920

// Part 2
let renderAllPixels =
    computeStatesFromFile
    >> Seq.chunkBySize 40
    >> Seq.iter (fun row ->
        row
        |> Array.map (fun (state: MachineState) -> state.pixelValue)
        |> System.String
        |> printfn "%s"
    )

renderAllPixels "test.txt"
renderAllPixels "in.txt" // BUCACBUZ
