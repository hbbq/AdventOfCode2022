module Day6


let Problem1 (input : string) = 

    let chars = input |> Common.Chars

    let index =
        chars
        |> Seq.windowed 4
        |> Seq.findIndex (fun e ->
            (e |> Array.distinct |> Array.length) = 4
        )

    index + 4

let Problem2 (input : string) = 

    let chars = input |> Common.Chars

    let index =
        chars
        |> Seq.windowed 14
        |> Seq.findIndex (fun e ->
            (e |> Array.distinct |> Array.length) = 14
        )

    index + 14