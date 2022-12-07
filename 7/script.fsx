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

type [<RequireQualifiedAccess>] Command =
    | ChangeDir of Target
    | List of ListResult list
and [<RequireQualifiedAccess>] ListResult =
    | Directory of name: string
    | File of size: int * name: string
and [<RequireQualifiedAccess>] Target =
    | Dir of string
    | UpOneLevel
    | Root
with
    static member ofString = function
        | "/" -> Target.Root
        | ".." -> Target.UpOneLevel
        | dir -> Target.Dir dir

[<RequireQualifiedAccess>]
type ParsedLine =
    | ChangeDir of dest: string
    | List
    | ListResultDir of name: string
    | ListResultFile of size: int * name: string
with
    static member ofString (s: string) =
        match s.Split(' ') with
        | [| "$"; "cd"; target |] -> ParsedLine.ChangeDir target
        | [| "$"; "ls" |] -> ParsedLine.List
        | [| "dir"; name |] -> ParsedLine.ListResultDir name
        | [| size; name |] ->
            match Int32.TryParse(size) with
            | (true, size') -> ParsedLine.ListResultFile (size', name)
            | (false, _) -> failwithf "Could not parse file size '%s' as int32 for file '%s'" size name
        | _ -> failwithf "Did not recognize line format: %s" s

let parse (s: string[]): Command list =
    let rec parse' (xs: string list) (state: Command list): Command list =
        match xs with
        | [] -> state
        | next :: rest ->
            match ParsedLine.ofString next, state with
            // `cd` and `ls` are simple cases, we always push a new state onto the state list
            | ParsedLine.ChangeDir dest, _ ->
                Command.ChangeDir (Target.ofString dest) :: state
            | ParsedLine.List, _ ->
                Command.List [] :: state
            // `ls` results are more tricky, we must ensure that an `ls` command is at the head of state list
            | ParsedLine.ListResultDir (dirName), (Command.List sibilings) :: commands ->
                // and then _replace_ the element at the head of the state list (cons with `commands`, not `state`)
                Command.List (ListResult.Directory(dirName) :: sibilings) :: commands
            | ParsedLine.ListResultFile (fileSize, fileName), (Command.List sibilings) :: commands ->
                // same as above, but we also need to extract the file size
                Command.List (ListResult.File(fileSize, fileName) :: sibilings) :: commands
            | ParsedLine.ListResultDir _, _
            | ParsedLine.ListResultFile _, _ ->
                failwithf "Invalid parser state: ls result cannot be parsed before ls command"
            |> parse' rest

    parse' (List.ofArray s) [] |> List.rev

let listFullyQualifiedFiles (commands: Command list) =
    let rec build (workingDir: string list) (state: Map<string list * string, int>) (xs: Command list) =
        match xs with
        | [] ->
            state
        | next :: rest ->
            match next with
            | Command.ChangeDir(Target.Dir dir) ->
                build (dir :: workingDir) state rest
            | Command.ChangeDir(Target.Root) ->
                build ["/"] state rest
            | Command.ChangeDir(Target.UpOneLevel) ->
                build (List.tail workingDir) state rest
            | Command.List contents ->
                let nextState =
                    contents
                    |> List.fold (fun st nxt ->
                        match nxt with
                        | ListResult.Directory _ -> st
                        | ListResult.File (size, name) ->
                            Map.add (workingDir, name) size st
                    ) state
                build workingDir nextState rest

    match commands with
    | Command.ChangeDir(Target.Root) :: rest ->
        // Assumption: first command is always `cd /`
        build ["/"] Map.empty rest
        |> Map.toList
    | [] ->
        failwith "Cannot construct graph from empty list"
    | _ -> failwith "We assume that the first command is always `cd /`"

let calcSizeByDir (files: ((string list * string) * int) list) =
    files
    |> List.fold (fun state ((parents, _), size) ->
        let rec calcSizes pathSegments sizeMap =
            match pathSegments with
            | [] -> sizeMap
            | _ ->
                let initialSize = Map.tryFind pathSegments sizeMap |> Option.defaultValue 0
                let newSizeMap = Map.add pathSegments (initialSize + size) sizeMap
                calcSizes (List.tail pathSegments) newSizeMap
        calcSizes parents state
    ) Map.empty
    |> Map.toList
    |> List.map (fun (path, size) -> List.rev path, size)

let processFilesystem f =
    IO.getAllLines
    >> parse
    >> listFullyQualifiedFiles
    >> calcSizeByDir
    >> f

// Part 1
let sumLargeDirs =
    List.map (fun (_, size) -> size)
    >> List.filter (fun size -> size <= 100000)
    >> List.sum

let testPartOne = Testing.assertOutput (processFilesystem sumLargeDirs)
testPartOne "test.txt" 95437
testPartOne "in.txt" 1444896

// Part 2
let sizeOfDirToDelete dirs =
    let totalSpaceUsed =
        dirs
        |> List.pick (fun (dirs, size) ->
            if dirs = ["/"] then
                Some size
            else
                None
        )

    let totalFree = 70000000 - totalSpaceUsed
    let needed = 30000000 - totalFree

    dirs
    |> List.filter (fun (_, size) -> size > needed)
    |> List.minBy (fun (_, size) -> size)
    |> (fun (_, size) -> size)

let testPartTwo = Testing.assertOutput (processFilesystem sizeOfDirToDelete)
testPartTwo "test.txt" 24933642
testPartTwo "in.txt" 404395
