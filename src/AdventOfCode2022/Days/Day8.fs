module Day8


let Problem1 (input : string) =

    let map = input |> Common.Lines |> Array.map (fun e -> 
        e |> Common.Digits |> Seq.toArray
    )

    let sy = map |> Array.length
    let sx = map |> Array.head |> Array.length
    
    let cnt = 
        seq {
            for y = 0 to sy - 1 do
                for x = 0 to sx - 1 do
                    let th = map[y][x]                
                    if 
                        map[y] |> Array.take (x) |> Array.exists (fun e -> e >= th)
                        && map[y] |> Array.skip (x+1) |> Array.exists (fun e -> e >= th)           
                        && map |> Array.take (y) |> Array.exists (fun e -> e[x] >= th)
                        && map |> Array.skip (y+1) |> Array.exists (fun e -> e[x] >= th)
                    then
                        yield 0
                    else
                        yield 1
        }

    cnt |> Seq.sum
        

let Problem2 (input : string) =

    let map = input |> Common.Lines |> Array.map (fun e -> 
        e |> Common.Digits |> Seq.toArray
    )

    let sy = map |> Array.length
    let sx = map |> Array.head |> Array.length
    
    let cnt = 
        seq {
            for y = 0 to sy - 1 do
                for x = 0 to sx - 1 do
                    let th = map[y][x]  
                    
                    let test (v:int) (a:int[]) =
                        let f = a |> Array.tryFindIndex (fun e -> e >= v)
                        match f with
                        | Some(x) -> x + 1
                        | None -> a |> Array.length 

                    let d1 = map[y] |> Array.take (x) |> Array.rev |> test th
                    let d2 = map[y] |> Array.skip (x+1) |> test th
                    let d3 = map |> Array.take (y) |> Array.rev |> Array.map(fun e -> e[x]) |> test th
                    let d4 = map |> Array.skip (y+1) |> Array.map(fun e -> e[x]) |> test th

                    yield d1*d2*d3*d4
        }

    cnt |> Seq.max