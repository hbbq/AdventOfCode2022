module Day3


let Problem1 (input : string) =

    let aval = (int) 'a'

    let Aval = (int) 'A'

    let calculateSack str =

        let chars = str |> Common.Chars |> Seq.toArray

        let size = (chars |> Array.length) / 2

        let comp1 = chars |> Array.take size

        let comp2 = chars |> Array.skip size

        let inBoth =
            comp1 |> Array.find (fun e -> (comp2 |> Array.contains e))

        let score =
            let cval = inBoth |> (int)
            if cval < aval then
                27 + cval - Aval
            else
                1 + cval - aval

        score

    let lines = input |> Common.Lines

    lines |> Array.sumBy calculateSack


let Problem2 (input : string) =

    let aval = (int) 'a'

    let Aval = (int) 'A'

    let lines = input |> Common.Lines

    let groups = lines |> Array.chunkBySize 3

    let calculateGroup (group : string[]) =
        let elfs = group |> Array.map Common.Chars
        
        let badge =
            elfs[0] |> Seq.find (fun e -> (elfs[1] |> Seq.contains e) && (elfs[2] |> Seq.contains e))
        
        let score =
            let cval = badge |> (int)
            if cval < aval then
                27 + cval - Aval
            else
                1 + cval - aval

        score

    groups |> Array.sumBy calculateGroup
