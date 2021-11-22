open System.IO
open System
open System.Security.Cryptography
open System.Text

let readLines filename = File.ReadAllLines filename

let md5 (d : string) : string =
    use md5 = MD5.Create()
    let data = Encoding.ASCII.GetBytes d
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string


[<EntryPoint>]
let main args =
    let filename =
        Array.tryItem 0 args
        |> Option.defaultValue "./input.txt"

    let lines = readLines filename
    printfn "Read %i lines from %s" lines.Length filename

    let passwordBase = lines.[0]

    let naturals = Seq.unfold (fun state -> Some(state, state + 1)) 0
    let passwordHashes1 = naturals
                            |> Seq.map (fun n -> md5 (passwordBase + string n))
                            |> Seq.filter (fun h -> h.[0..4] = "00000")

    let password1 = passwordHashes1
                        |> Seq.take 8
                        |> Seq.map (fun c -> string c.[5])
                        |> String.concat ""

    printfn "Part 1: %s" password1

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let passwordHashes2 = passwordHashes1
                            |> Seq.filter (fun e -> (int e.[5] - int '0') < 8)
                            |> Seq.map (fun e -> (int e.[5], e))
                            |> Seq.distinctBy (fun e -> fst(e))
                            |> Seq.take 8

    let password2Chars = passwordHashes2 |> Seq.map (fun c -> (fst(c), snd(c).[6])) |> Seq.sort
    let password2 = password2Chars |> Seq.map (fun cp -> snd(cp) |> string) |> String.concat ""

    printfn "Part 2: %s" password2

    stopWatch.Stop()
    printfn "%fs" (stopWatch.Elapsed.TotalMilliseconds / 60.0)

    0
