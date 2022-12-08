module adventofcodeday7

open Xunit
open FsUnit.Xunit
open System
open System.IO
open System.Collections.Generic
open Utils

type InputLine =
    | Cd of string
    | Ls
    | DirectoryOutput of string
    | File of (int * string)

let (|StartsWith|_|) (pattern: string) (str: string) =
    if str.StartsWith pattern
    then Some <| str[pattern.Length..].Trim()
    else None

let parseLine = function
    | StartsWith "$ cd " dir -> Cd dir
    | "$ ls" -> Ls
    | StartsWith "dir " dir -> DirectoryOutput dir
    | s -> 
        match s.Split " " with
        | [| size; name |] -> File (int size, name)
        | _ -> raise (Exception "Did not parse file size properly")

type Directory = {
    Parent: Directory Option
    Subdirectories: Dictionary<string, Directory>
    Files: Dictionary<string, int>
}

let addDirectory (parent: Directory) (path: string) =
    let dir = {
        Parent = Some parent
        Subdirectories = new Dictionary<string, Directory>()
        Files = new Dictionary<string, int>()
    }
    parent.Subdirectories.Add(path, dir)

let findDirectory (parent: Directory) (path: string) =
    match parent.Subdirectories.TryGetValue path with
    | (true, dir) -> dir
    | (false, _) -> raise (Exception "Traversed into unknown directory")

let accumulator (root: Directory, cwd: Directory) = function
    | Ls -> (root, cwd)
    | Cd ".." -> (root, cwd.Parent.Value)
    | Cd "/" -> (root, root)
    | Cd path -> (root, findDirectory cwd path)
    | DirectoryOutput dir ->
        addDirectory cwd dir
        (root, cwd)
    | File (size, name)->
        cwd.Files[name] <- size
        (root, cwd)

let blankFileSystem () =
    {
        Parent = None
        Subdirectories = new Dictionary<string, Directory>()
        Files = new Dictionary<string, int>()
    }

let parseFile (filename: string) =
    let root = blankFileSystem ()
    let input =
        File.ReadLines filename
        |> Seq.map parseLine

    input
    |> Seq.fold accumulator (root, root)
    |> Operators.fst

let rec sumDirectorySizeImpl (dir: Directory) =
    let childrenSize = dir.Subdirectories.Values |> Seq.sumBy sumDirectorySizeImpl
    let fileSizes = dir.Files.Values |> Seq.sum
    fileSizes + childrenSize

let sumDirectorySize = memoize sumDirectorySizeImpl

let combinePath (cwd: string) (name: string) =
    if cwd = "/"
    then $"/{name}"
    else $"{cwd}/{name}"

let rec listDirectories (dir: Directory) (path: string) =
    seq {
        yield (path, sumDirectorySize dir)
        for kvp in dir.Subdirectories do
            yield! listDirectories kvp.Value (combinePath path kvp.Key)
    }

[<Fact>]
let Challenge7sample () =
    let root = parseFile "day7-sample.txt"
    listDirectories root "/"
    |> Seq.filter (fun (_, size) -> size <= 100000)
    |> Seq.sumBy Operators.snd
    |> should equal 95437

[<Fact>]
let Challenge7 () =
    let root = parseFile "day7.txt"
    listDirectories root "/"
    |> Seq.filter (fun (_, size) -> size <= 100000)
    |> Seq.sumBy Operators.snd
    |> should equal 1306611

[<Fact>]
let Challenge7a () =
    let root = parseFile "day7.txt"
    let sizeToFree = (sumDirectorySize root) - 40000000

    listDirectories root "/"
    |> Seq.filter (fun (_, size) -> size >= sizeToFree)
    |> Seq.sortBy Operators.snd
    |> Seq.head
    |> should equal ("/hfm/hfm/fst", 13210366)
