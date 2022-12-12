module Day9


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let positions =
        seq {
        
            let mutable tx = 0
            let mutable ty = 0
            let mutable hx = 0
            let mutable hy = 0

            yield (tx,ty)

            for line in lines do

                let parts = line |> Common.Words
                let times = parts[1] |> int

                let (dx, dy) = 
                    match parts[0] with
                    | "R" -> (1,0)
                    | "L" -> (-1,0)
                    | "U" -> (0,1)
                    | "D" -> (0,-1)
                    | _ -> (0,0)

                for i = 1 to times do

                    hx <- hx + dx
                    hy <- hy + dy

                    let rx =
                        if hx > tx then 1
                        else if hx < tx then -1
                        else 0

                    let ry =
                        if hy > ty then 1
                        else if hy < ty then -1
                        else 0

                    if abs(hx-tx) > 1 || abs(hy - ty) > 1 then
                        tx <- tx + rx
                        ty <- ty + ry
                        yield(tx, ty)

        }

    positions |> Seq.distinct |> Seq.length


let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let positions =
        seq {
        
            let mutable tx = [|0;0;0;0;0;0;0;0;0;0|]
            let mutable ty = [|0;0;0;0;0;0;0;0;0;0|]

            yield (tx[9],ty[9])

            for line in lines do

                let parts = line |> Common.Words
                let times = parts[1] |> int

                let (dx, dy) = 
                    match parts[0] with
                    | "R" -> (1,0)
                    | "L" -> (-1,0)
                    | "U" -> (0,1)
                    | "D" -> (0,-1)
                    | _ -> (0,0)

                for i = 1 to times do

                    tx[0] <- tx[0] + dx
                    ty[0] <- ty[0] + dy

                    for k = 1 to 9 do

                        let rx =
                            if tx[k-1] > tx[k] then 1
                            else if tx[k-1] < tx[k] then -1
                            else 0

                        let ry =
                            if ty[k-1] > ty[k] then 1
                            else if ty[k-1] < ty[k] then -1
                            else 0

                        if abs(tx[k-1]-tx[k]) > 1 || abs(ty[k-1]-ty[k]) > 1 then
                            tx[k] <- tx[k] + rx
                            ty[k] <- ty[k] + ry

                    yield (tx[9],ty[9])

        }

    positions |> Seq.distinct |> Seq.length