module Day1


let Problem1 (input : string) =
        
    let groups = input.Replace("\n", "\r").Replace("\r\r", "\r").Split("\r\r")

    let sum group =
        group |> Common.Lines |> Array.sumBy (int)

    groups |> Array.map sum |> Array.max


let Problem2 (input : string) =

    let groups = input.Replace("\n", "\r").Replace("\r\r", "\r").Split("\r\r")

    let sum group =
        group |> Common.Lines |> Array.sumBy (int)

    groups |> Array.map sum |> Array.sortDescending |> Array.take 3 |> Array.sum