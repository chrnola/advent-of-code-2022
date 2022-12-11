module IO =
    open System.IO
    let getAllLines fileName = Path.Join(__SOURCE_DIRECTORY__, fileName) |> File.ReadAllLines

module Testing =
    /// Passes the given `file` to `f` and validates that `f` yields `expected`, otherwise throws.
    let assertOutput f file expected =
        let actual = f file
        if actual <> expected then
            failwithf "Assertion failed! Expected %i, got %i" expected actual

/// Allows us to derive new coordinates by addition
let inline (+>) ((a1, a2)) ((b1, b2))=
    (a1 + b1), (a2 + b2)

module Vector =
    let Zero = (0, 0)
    let Right = (1, 0)
    let Left = (-1, 0)
    let Up = (0, 1)
    let Down = (0, -1)
    let NE = Up +> Right
    let NW = Up +> Left
    let SE = Down +> Right
    let SW = Down +> Left

    let All = seq {
        yield Zero
        yield Right; yield Left; yield Up; yield Down
        yield NE; yield NW; yield SE; yield SW
    }

[<RequireQualifiedAccess>]
type Direction = Right | Left | Up | Down
with
    static member ofString = function
        | "R" -> Right
        | "L" -> Left
        | "U" -> Up
        | "D" -> Down
        | x -> failwithf "Could not parse direction '%s', expecting 'L', 'R', 'U', or 'D'" x

    static member Vector = function
        | Right -> Vector.Right
        | Left -> Vector.Left
        | Up -> Vector.Up
        | Down -> Vector.Down

type Motion =
    { direction: Direction
      steps: int }
with
    static member ofString (s: string) =
        match s.Split(' ') with
        | [| dir; steps |] ->
            { Motion.direction = Direction.ofString dir
              steps = System.Int32.Parse(steps) }
        | _ ->
            failwithf "Could not parse motion '%s'" s

    static member flatten m =
        List.init (m.steps) (fun _ -> m.direction)

/// Determines if the given coordinate pairs, `a` and `b`, are overlapping or diagonally adjacent to each other.
let areTouching a b =
    Seq.exists(fun vec -> (b +> vec) = a) Vector.All

/// Active pattern to express the relative position of one knot to another in the X dimension.
let (|SameX|LeftOfTarget|RightOfTarget|) ((theirX, _), (ourX, _)) =
    if theirX = ourX then
        SameX
    elif theirX > ourX then
        LeftOfTarget
    else
        RightOfTarget

/// Active pattern to express the relative position of one knot to another in the Y dimension.
let (|SameY|BelowTarget|AboveTarget|) ((_, theirY), (_, ourY)) =
    if theirY = ourY then
        SameY
    elif theirY > ourY then
        BelowTarget
    else
        AboveTarget

/// Determines the position of tail knots relative to how their predecessor moved
/// returning a list of ALL knots in the chain in reverse order. The head knot's new
/// position is expected to be wrapped in a singleton list and used to initialize the
/// accumulator list.
let rec updateTailKnots knots acc =
    match knots, acc with
    | _, [] ->
        failwith "Accumulator state cannot ever be empty!"
    | [], acc ->
        acc // Base case: No more tail knots to process
    | knot :: rest, prev :: _ ->
        let knot' =
            if areTouching prev knot then
                knot // Don't move
            else
                match (prev, knot) with
                | SameX & SameY -> Vector.Zero // Should never happen since they're not touching
                | SameX & BelowTarget-> Vector.Up
                | SameX & AboveTarget -> Vector.Down
                | LeftOfTarget & SameY -> Vector.Right
                | RightOfTarget & SameY-> Vector.Left
                | LeftOfTarget & BelowTarget -> Vector.NE
                | RightOfTarget & BelowTarget -> Vector.NW
                | LeftOfTarget & AboveTarget -> Vector.SE
                | RightOfTarget & AboveTarget -> Vector.SW
                |> (+>) knot

        updateTailKnots rest (knot' :: acc)

/// Computes next position relative to initial starting position/origin/(0,0)
let applyMotions numKnots =
    List.fold (fun (knots, uniqueCoords: Set<int*int>) dir ->
        // Incoming direction only applies to the head knot
        let newHead = List.head knots +> Direction.Vector dir

        // The remaining knots should only update relative to their predecessor, not the direction
        let stonk = updateTailKnots (List.tail knots) [ newHead ]

        // Before we reverse the knots list, the head will point to the last tail knot,
        // so add it to the set of unique coordinates in case it's new
        let uniqueCoords' = Set.add (List.head stonk) uniqueCoords

        // lol get it? stonk = knots backwards because it was accumulated in reverse order
        List.rev stonk, uniqueCoords'
    ) (List.init numKnots (fun _ -> (0,0)), Set.add (0,0) Set.empty)

let countDistinctTailCoords knots =
    IO.getAllLines
    >> List.ofArray
    >> List.collect (Motion.ofString >> Motion.flatten)
    >> applyMotions knots
    >> (fun (_, s) -> Set.count s) // We just want the distinct tail points

let testPartOne = Testing.assertOutput (countDistinctTailCoords 2)
testPartOne "test.txt" 13
testPartOne "in.txt" 6271

let testPartTwo = Testing.assertOutput (countDistinctTailCoords 10)
testPartTwo "test.txt" 1
testPartTwo "test2.txt" 36
testPartTwo "in.txt" 2458
