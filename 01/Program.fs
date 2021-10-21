open System.IO
open System

let readLines filename = File.ReadAllLines filename



type Movement =
    | Left
    | Right

type Direction =
    | Up = 0
    | Right = 1
    | Down = 2
    | Left = 3

type Move = Movement * int

let getMovement ch =
    match ch with
    | 'L' -> Movement.Left
    | 'R' -> Movement.Right
    | _ -> failwithf "Error: Wrong movement = %c" ch

type Point = int * int
type Position = int * int * Direction

let moveOffset position direction offset =
    let (x, y, _) = position

    match direction with
    | Direction.Up -> (x, y + offset, direction)
    | Direction.Right -> (x + offset, y, direction)
    | Direction.Down -> (x, y - offset, direction)
    | Direction.Left -> (x - offset, y, direction)
    | _ -> failwithf "Error: Wrong direction = %i" (int direction)

let moveLeft direction =
    (int direction + 3) % 4 |> enum<Direction>

let moveRight direction =
    (int direction + 1) % 4 |> enum<Direction>

let move moveData position =
    let (turn, offset) = moveData
    let (_, _, direction) = position

    match turn with
    | Movement.Left -> moveOffset position (moveLeft direction) offset
    | Movement.Right -> moveOffset position (moveRight direction) offset

let distance moves pos =
    (pos, moves) ||> Array.fold (fun p m -> move m p)

let getSingleMoves move pos =
    let (_, _, direction) = pos
    let (turn, offset) = move

    let newDirection =
        match turn with
        | Movement.Left -> moveLeft direction
        | Movement.Right -> moveRight direction

    let points, lastPoint =
        (pos, seq { for i in 1 .. offset -> i })
        ||> Seq.mapFold
                (fun state elem ->
                    let np = moveOffset state newDirection 1
                    np, np)

    points

let recordVisitedPoints moves pos =
    let allPoints, _ =
        (pos, moves)
        ||> Array.mapFold
                (fun p m ->
                    let np = move m p
                    getSingleMoves m p, np)

    let flattenAllPoints =
        allPoints
        |> Seq.concat
        |> Seq.map
            (fun e ->
                let (x, y, d) = e
                (x, y))

    // flattenAllPoints |> Seq.iter (printfn "%O")

    let (_, repeatedPoint) =
        flattenAllPoints
        |> Seq.indexed
        |> Seq.skip 1
        |> Seq.find
            (fun elem ->
                let index, e = elem

                flattenAllPoints
                |> Seq.take (index - 1)
                |> Seq.contains e)

    repeatedPoint

[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let moves =
        lines.[0]
            .Split([| ", " |], StringSplitOptions.None)
        |> Array.map (fun str -> (getMovement str.[0], str.[1..] |> int))

    let initialPoint = (0, 0, Direction.Up)

    let movePoint = distance moves initialPoint
    let (mPx, mPy, _) = movePoint

    // printfn "Final point part1: %O" movePoint

    printfn "Part 1: %i" ((abs mPx) + (abs mPy))

    let firstRepeatedMove = recordVisitedPoints moves initialPoint

    let (frPx, frPy) = firstRepeatedMove

    // printfn "Final point part2: %O" firstRepeatedMove

    printfn "Part 2: %i" ((abs frPx) + (abs frPy))

    0
