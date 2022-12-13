module Day10


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let parseLine line =
        let parts = line |> Common.Words
        match parts[0] with
        | "addx" -> (2, (int)parts[1])
        | _ -> (1, 0)

    let running = 
        lines 
        |> Array.map parseLine 
        |> Array.scan (fun a e -> (fst a + fst e, snd a + snd e)) (0, 1)

    [|20;60;100;140;180;220|]
    |> Array.map (fun e ->
        running 
        |> Array.where (fun x -> fst x < e)
        |> Array.map (fun x -> snd x * e)
        |> Array.last
    )
    |> Array.sum


let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let parseLine line =
        let parts = line |> Common.Words
        match parts[0] with
        | "addx" -> (2, (int)parts[1])
        | _ -> (1, 0)

    let running = 
        lines 
        |> Array.map parseLine 
        |> Array.scan (fun a e -> (fst a + fst e, snd a + snd e)) (0, 1)

    let sb = new System.Text.StringBuilder()
    
    seq {
        for y = 0 to 5 do
            yield "\n"
            for x = 0 to 39 do
                let p = running |> Array.where (fun t -> fst t <= y * 40 + x) |> Array.tryLast
                let pos = 
                    match p with
                    | Some(x) -> snd x
                    | None -> 1
                let c =
                    if pos > x - 2 && pos < x + 2 then "#" else "."
                yield c
    }
    |> System.String.Concat
