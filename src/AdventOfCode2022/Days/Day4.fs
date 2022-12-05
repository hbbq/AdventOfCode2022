module Day4


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let getPairs (line : string) =
        line.Split(',')
        |> Array.map (fun e -> e.Split('-') |> Array.map (int))
        
    let checkPairs line =
        let nums = line |> getPairs
        let c = (nums[0][0] - nums[1][0]) * (nums[0][1] - nums[1][1])
        if c <= 0 then 1 else 0

    lines |> Array.sumBy checkPairs


let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let getPairs (line : string) =
        line.Split(',')
        |> Array.map (fun e -> e.Split('-') |> Array.map (int))
        
    let checkPairs line =
        let nums = line |> getPairs
        if 
            (nums[0] |> Array.min) <= (nums[1] |> Array.max)
            &&
            (nums[1] |> Array.min) <= (nums[0] |> Array.max)
            then 1 else 0

    lines |> Array.sumBy checkPairs