open System.IO
open System

let readLines filename = File.ReadAllLines filename

type triangle = int * int * int

let isValid triangle =
    let triangleSort = triangle |> Array.sort

    triangleSort.[0] + triangleSort.[1] > triangleSort.[2]

[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let triangles =
        lines
        |> Array.map
            (fun elem ->
                elem
                    .Trim()
                    .Split([| "  " |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun e -> int e))

    let validTriangle =
        triangles
        |> Array.filter (fun triangle -> isValid triangle)

    printfn "Part 1: %i" validTriangle.Length

    let listTriangles = seq { 0..2 } |> Seq.map (fun index -> triangles |> Array.collect (fun e -> [|e.[index]|])) |> Seq.concat |> Seq.toArray

    // listTriangles |> Array.iter (fun e -> printfn "%i, " e)

    let transposeTriangles = listTriangles |> Array.chunkBySize 3

    let validTransposeTriangles = transposeTriangles |> Array.filter (fun triangle -> isValid triangle)

    printfn "Part 2: %i" validTransposeTriangles.Length

    0
