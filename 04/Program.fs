open System.IO
open System

let readLines filename = File.ReadAllLines filename

let getId (roomData: string[]) =
    match (roomData |> Array.tryLast) with
       | Some(x) -> x
       | _ -> failwithf "Error: Room id"

let getCrc (roomData: string[]) =
    let id = getId roomData
    let crc2 = (id.Split('[') |> Array.skip 1 |> Array.take 1).[0]
    let crc = (crc2.Split(']') |> Array.take 1).[0]
    crc

let getNumber (roomData: string[]) =
    let id = getId roomData
    int (id.Split('[') |> Array.take 1).[0]

let isValid (roomData: string[]) =
    let roomsChars = seq { 0..(roomData.Length - 2) } |> Seq.map (fun e -> roomData.[e]) |> Seq.concat
    // roomsChars |> Seq.iter (fun a -> printfn "%c" a)
    // printfn "%s" id
    let crc = getCrc roomData
    let uniqueChars = roomsChars |> Seq.map (fun c -> roomsChars |> Seq.countBy (fun e -> e)) |> Seq.concat |> Seq.distinct
    let sortUniqueChars = uniqueChars |> Seq.sortBy (fun e -> (-snd(e), fst(e)))
    let calcCrc = sortUniqueChars |> Seq.take crc.Length |> Seq.map (fun e -> string (fst(e))) |> String.concat ""
    crc = calcCrc

let decrypt (roomName: string) (roomSector: int) =
    let abecedario = "abcdefghijklmnopqrstuvwxyz"
    let decryptedName = roomName |> String.map (fun c -> match abecedario.IndexOf(c) with
                                                            | -1 -> ' '
                                                            | idx -> abecedario.[(idx + roomSector) % 26])
    decryptedName

let searchRoom (roomData: string[]) =
    let sectorId = getNumber roomData
    let roomDecryptedName = seq { 0..(roomData.Length - 2) } |> Seq.map (fun e -> decrypt roomData.[e] sectorId) |> String.concat " "
    (roomDecryptedName, sectorId)


[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let rooms =
        lines
        |> Array.map
            (fun elem ->
                elem
                    .Trim()
                    .Split('-'))

    // rooms |> Array.iter (fun e -> e |> Array.iter (fun a -> printf "%s-" a); printfn "")

    let validRooms = rooms |> Array.filter (fun e -> isValid e)

    let solution1 = validRooms |> Array.map (fun e -> getNumber e) |> Array.reduce (fun a b -> a + b)

    printfn "Part 1: %i" solution1

    let decryptedRooms = validRooms |> Array.map (fun e -> searchRoom e)

    let nortPole = decryptedRooms |> Array.find (fun e -> fst(e).Contains("northpole object storage") )

    printfn "Part 2: %i" (snd(nortPole))

    0
