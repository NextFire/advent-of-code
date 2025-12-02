open System.IO

let input = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"

input
|> Array.map (fun x ->
    x.Split '-'
    |> Array.map int64
    |> fun brackets -> seq { brackets[0] .. brackets[1] } |> Seq.map string
    |> Seq.filter (fun (id: string) -> id.Length % 2 = 0 && id[0 .. id.Length / 2 - 1] = id[id.Length / 2 ..])
    |> Seq.sumBy int64)
|> Seq.sum
|> printfn "%A"

let isInvalid (id: string) =
    seq {
        for patternLength in 1 .. id.Length / 2 do
            if id.Length % patternLength = 0 then
                patternLength
    }
    |> Seq.exists (fun patternLength ->
        seq { patternLength..patternLength .. id.Length - patternLength }
        |> Seq.forall (fun start -> id[0 .. patternLength - 1] = id[start .. start + patternLength - 1]))

input
|> Array.map (fun x ->
    x.Split '-'
    |> Array.map int64
    |> fun brackets -> seq { brackets[0] .. brackets[1] } |> Seq.map string
    |> Seq.filter isInvalid
    |> Seq.sumBy int64)
|> Seq.sum
|> printfn "%A"
