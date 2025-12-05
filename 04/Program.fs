open System.IO

let input = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"

let rec neighborPositions (maxi, maxj) (i, j) =
    seq { -1 .. 1 }
    |> Seq.map (fun k -> seq { -1 .. 1 } |> Seq.map (fun l -> i + k, j + l))
    |> Seq.concat
    |> Seq.filter (fun (ni, nj) -> ni >= 0 && ni <= maxi && nj >= 0 && nj <= maxj && (ni, nj) <> (i, j))
    |> Seq.toArray

input
|> Array.mapi (fun i l ->
    Seq.mapi
        (fun j c ->
            if c = '@' then
                ([], neighborPositions (input.Length - 1, l.Length - 1) (i, j))
                ||> Array.fold (fun acc (ni, nj) -> if input[ni][nj] = '@' then acc @ [ (ni, nj) ] else acc)
                |> Some
            else
                None)
        l)
|> Array.sumBy (
    Seq.fold
        (fun acc opt ->
            match opt with
            | Some l -> if List.length l < 4 then acc + 1 else acc
            | None -> acc)
        0
)
|> printfn "%A"

let rollsNeighbors (map: char array array) =
    map
    |> Array.mapi (fun i l ->
        Array.mapi
            (fun j c ->
                if c = '@' then
                    ([], neighborPositions (map.Length - 1, l.Length - 1) (i, j))
                    ||> Array.fold (fun acc (ni, nj) -> if map[ni][nj] = '@' then acc @ [ (ni, nj) ] else acc)
                    |> Some
                else
                    None)
            l)

let countForkliftRolls =
    Array.sumBy (
        Array.fold
            (fun acc opt ->
                match opt with
                | Some l -> if List.length l < 4 then acc + 1 else acc
                | None -> acc)
            0
    )

let debugMap map =
    for l in map do
        for c in l do
            printf "%c" c

        printfn ""

    printfn ""

let rec recCountForkliftRolls map =
    let rolls = rollsNeighbors map

    let newMap =
        rolls
        |> Array.map (
            Array.map (fun opt ->
                match opt with
                | Some l -> if List.length l < 4 then '.' else '@'
                | None -> '.')
        )

    // debugMap newMap

    countForkliftRolls rolls
    + if newMap <> map then recCountForkliftRolls newMap else 0


input
|> Array.map (fun l -> l.ToCharArray())
|> recCountForkliftRolls
|> printfn "%A"
