open System.IO

let input = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"

input
|> Seq.map (
    Seq.map (fun c -> int c - int '0')
    >> fun digits ->
        Seq.foldBack
            (fun digit (first, second) ->
                match first, second with
                | 0, 0 -> 0, digit
                | 0, _ -> digit, second
                | _, _ when digit >= first -> digit, if first > second then first else second
                | _, _ -> first, second)
            digits
            (0, 0)
)
|> Seq.sumBy (fun digits -> digits ||> sprintf "%d%d" |> int)
|> printfn "%A"

let rec rebuild digit jolts =
    match jolts with
    | [] -> []
    | h :: t -> if digit >= h then digit :: rebuild h t else jolts

input
|> Seq.map (
    Seq.map (fun c -> int c - int '0')
    >> fun digits ->
        Seq.foldBack
            (fun digit jolts ->
                let zero = Seq.tryFindIndexBack (fun d -> d = 0) jolts

                match zero with
                | Some i -> jolts[.. i - 1] @ [ digit ] @ jolts[i + 1 ..]
                | None -> rebuild digit jolts)
            digits
            (List.init 12 (fun _ -> 0))
)
|> Seq.sumBy (List.fold (fun acc d -> acc * 10UL + uint64 d) 0UL)
|> printfn "%A"
