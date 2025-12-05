open System.IO

let rangesInput = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.0.txt"
let ingredientsInput = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.1.txt"

let fresh =
    rangesInput
    |> Array.map (fun l -> l.Split '-' |> Array.map uint64 |> fun b -> b[0], b[1])

ingredientsInput
|> Array.map uint64
|> Array.sumBy (fun i ->
    if Array.exists (fun (s, e) -> s <= i && i <= e) fresh then
        1
    else
        0)
|> printfn "%A"

((0UL, 0UL), fresh |> Array.sort)
||> Array.fold (fun (prevEnd, count) (currStart, currEnd) ->
    let start = max (prevEnd + 1UL) currStart
    let toAdd = if currEnd >= start then currEnd - start + 1UL else 0UL
    // printfn "%A" (start, currEnd, toAdd)
    max prevEnd currEnd, count + toAdd)
|> snd
|> printfn "%d"
