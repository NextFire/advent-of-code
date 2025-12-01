open System.IO

let input = File.ReadAllLines $"{__SOURCE_DIRECTORY__}/input.txt"

((50, 0), input)
||> Array.scan (fun (pos, count) x ->
    let incr = (if x[0] = 'L' then -1 else 1) * int x[1..]
    let newPos = pos + incr
    newPos, count + (if newPos % 100 = 0 then 1 else 0))
|> Array.last
|> printfn "%A"

let countZeros pos incr =
    match pos, incr with
    | _ when pos = 0 -> abs incr / 100
    | _ when incr < 0 && abs incr >= pos -> 1 + (abs incr - pos) / 100
    | _ when incr > 0 && incr >= 100 - pos -> 1 + (incr - (100 - pos)) / 100
    | _ -> 0

((50, 0), input)
||> Array.scan (fun (pos, count) x ->
    let incr = (if x[0] = 'L' then -1 else 1) * int x[1..]

    let newPos =
        let modulo = (pos + incr) % 100
        modulo + if modulo < 0 then 100 else 0

    newPos, count + countZeros pos incr)
|> Array.last
|> printfn "%A"
