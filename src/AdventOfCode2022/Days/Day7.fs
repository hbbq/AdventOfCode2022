module Day7


let Problem1 (input : string) =

    let lines = input |> Common.Lines

    let commands = new System.Collections.Generic.Queue<string>(lines)

    let folders = new System.Collections.Generic.List<(string * int64)>();

    let rec trav (folder : string) =
        
        let mutable ex = false
        let mutable thisSize = 0L
        
        while (not ex) && commands.Count > 0 do
            
            let tc = commands.Dequeue()

            let parts = tc |> Common.Words

            if parts[0] = "dir" then
                ()
            else
            if parts[0] = "$" then
                if parts[1] = "cd" then
                    if parts[2] = ".." then
                        ex <- true
                    else
                        let sz = trav parts[2]
                        thisSize <- thisSize + sz
            else
                let sz = parts[0] |> int64
                thisSize <- thisSize + sz

        folders.Add((folder, thisSize))

        thisSize

    let z = trav "\\"

    folders.ToArray() |> Array.map snd |> Array.where (fun e -> e <= 100000L) |> Array.sum


let Problem2 (input : string) =

    let lines = input |> Common.Lines

    let commands = new System.Collections.Generic.Queue<string>(lines)

    let folders = new System.Collections.Generic.List<(string * int64)>();

    let rec trav (folder : string) =
        
        let mutable ex = false
        let mutable thisSize = 0L
        
        while (not ex) && commands.Count > 0 do
            
            let tc = commands.Dequeue()

            let parts = tc |> Common.Words

            if parts[0] = "dir" then
                ()
            else
            if parts[0] = "$" then
                if parts[1] = "cd" then
                    if parts[2] = ".." then
                        ex <- true
                    else
                        let sz = trav parts[2]
                        thisSize <- thisSize + sz
            else
                let sz = parts[0] |> int64
                thisSize <- thisSize + sz

        folders.Add((folder, thisSize))

        thisSize

    let z = trav "\\"

    let needed = 30000000L + (folders.ToArray() |> Array.map snd |> Array.max) - 70000000L

    folders.ToArray() |> Array.map snd |> Array.sort |> Array.find (fun e -> e >= needed)
                