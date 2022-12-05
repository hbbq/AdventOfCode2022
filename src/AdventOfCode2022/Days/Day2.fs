module Day2


let Problem1 (input : string) =

    let mapOpp c =
        match c with
            | 'A' -> 1
            | 'B' -> 2
            | 'C' -> 3

    let mapMe c =
        match c with
            | 'X' -> 1
            | 'Y' -> 2
            | 'Z' -> 3

    let matchScore opp me =
        let oppNum = opp |> mapOpp
        let meNum = me |> mapMe
        if meNum = oppNum then
            meNum + 3
        else
            match (3 + meNum - oppNum) % 3 with
                | 1 -> meNum + 6
                | 2 -> meNum
                | _ -> 0

    input |> Common.Lines |> Array.map (fun e ->
        matchScore e[0] e[2]
    ) |> Array.sum
        

let Problem2 (input : string) =

    let mapOpp c =
        match c with
            | 'A' -> 0
            | 'B' -> 1
            | 'C' -> 2

    let matchScore opp me =
        let oppNum = opp |> mapOpp
        let meNum = 
            (
                match me with
                    | 'X' -> oppNum + 2
                    | 'Y' -> oppNum
                    | 'Z' -> oppNum + 1
            ) % 3

        if meNum = oppNum then
            meNum + 1 + 3
        else
            match (3 + meNum - oppNum) % 3 with
                | 1 -> meNum + 1 + 6
                | 2 -> meNum + 1
                | _ -> 0

    input |> Common.Lines |> Array.map (fun e ->
        matchScore e[0] e[2]
    ) |> Array.sum