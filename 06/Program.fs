open System.IO
open System
open System.Security.Cryptography
open System.Text

let readLines filename = File.ReadAllLines filename

let selectFirst letters =
    letters
        |> Seq.map (fun r -> r |> Seq.head |> fst |> string)
        |> String.concat ""

[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let possiblesCol1 = seq { 0..lines.[0].Length - 1 }
                        |> Seq.map (fun r ->
                                            lines
                                            |> Array.map (fun rs -> string rs.[r])
                                            |> Seq.concat)

    let lettersFreq = possiblesCol1
                        |> Seq.map (fun c -> c
                                            |> Seq.countBy (fun cc -> cc)
                                            |> Seq.distinct)

    let bestCol1 = lettersFreq
                    |> Seq.map (fun c -> c
                                            |> Seq.sortBy (fun p -> (-snd(p), fst(p))))

    let message = bestCol1 |> selectFirst

    printfn "Part 1: %s" message

    let bestCol2 = lettersFreq
                    |> Seq.map (fun c -> c
                                            |> Seq.sortBy (fun p -> (snd(p), fst(p))))

    let message2 = bestCol2 |> selectFirst

    printfn "Part 1: %s" message2

    0
