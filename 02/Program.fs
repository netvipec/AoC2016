open System.IO
open System

let readLines filename = File.ReadAllLines filename


type Position = int * int

type Direction =
    | Up = 0
    | Right = 1
    | Down = 2
    | Left = 3

let getDirection ch =
    match ch with
    | 'L' -> Direction.Left
    | 'R' -> Direction.Right
    | 'D' -> Direction.Down
    | 'U' -> Direction.Up
    | _ -> failwithf "Error: Wrong movement = %c" ch

let moveOffset1 position direction =
    let (x, y) = position

    match direction with
    | Direction.Up -> if y < 1 then (x, y + 1) else (x, y)
    | Direction.Right -> if x < 1 then (x + 1, y) else (x, y)
    | Direction.Down -> if y > -1 then (x, y - 1) else (x, y)
    | Direction.Left -> if x > -1 then (x - 1, y) else (x, y)
    | _ -> failwithf "Error: Wrong direction = %i" (int direction)

let moveOffset2 position direction =
    let (x, y) = position

    match direction with
    | Direction.Up ->
        if (abs x) + (abs (y + 1)) > 2 then
            (x, y)
        else
            (x, y + 1)
    | Direction.Right ->
        if (abs (x + 1)) + (abs y) > 2 then
            (x, y)
        else
            (x + 1, y)
    | Direction.Down ->
        if (abs x) + (abs (y - 1)) > 2 then
            (x, y)
        else
            (x, y - 1)
    | Direction.Left ->
        if (abs (x - 1)) + (abs y) > 2 then
            (x, y)
        else
            (x - 1, y)
    | _ -> failwithf "Error: Wrong direction = %i" (int direction)

let calculatePhoneNumber1 position =
    let (x, y) = position

    5 - y * 3 + x

let calculatePhoneNumber2 position =
    let (x, y) = position

    let result = 7 - y * 3 + x

    if (abs y) = 1 then
        result - y
    else
        result

let code1 (movement: String []) position =
    let movePosition, other =
        (position, movement)
        ||> Seq.mapFold
                (fun pos movementLine ->
                    let finalPos =
                        (pos, movementLine)
                        ||> Seq.fold
                                (fun posInner movementChar ->
                                    let movementEnum = getDirection movementChar
                                    moveOffset1 posInner movementEnum)

                    let codeDigit = calculatePhoneNumber1 finalPos
                    (codeDigit, finalPos))

    movePosition
    |> Seq.map (fun elem -> string elem)
    |> String.concat ""

let code2 (movement: String []) position =
    let movePosition, other =
        (position, movement)
        ||> Seq.mapFold
                (fun pos movementLine ->
                    let finalPos =
                        (pos, movementLine)
                        ||> Seq.fold
                                (fun posInner movementChar ->
                                    let movementEnum = getDirection movementChar
                                    moveOffset2 posInner movementEnum)

                    let codeDigit = calculatePhoneNumber2 finalPos
                    (codeDigit, finalPos))

    movePosition
    |> Seq.map
        (fun elem ->
            if elem < 10 then
                string elem
            else
                string "ABCD".[elem - 10])
    |> String.concat ""

[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let initialPoint1 = (0, 0)

    let bathroomCode1 = code1 lines initialPoint1

    // printfn "Final point part1: %O" bathroomCode1

    printfn "Part 1: %O" bathroomCode1

    let initialPoint2 = (-2, 0)

    let bathroomCode2 = code2 lines initialPoint2

    // printfn "Final point part2: %O" bathroomCode2

    printfn "Part 2: %s" bathroomCode2

    0
