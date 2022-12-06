module Day5


let Problem1 (input : string) =

    let groups = input.Replace("\n", "\r").Replace("\r\r", "\r").Split("\r\r")

    let stackLines = 
        groups[0].Replace("\n", "\r").Replace("\r\r", "\r").Split('\r') 

    let fixedStackLines = stackLines |> Array.take ((stackLines |> Array.length) - 1)

    let preStacks = new System.Collections.Generic.List<System.Collections.Generic.List<char>>()

    for line in fixedStackLines do
        let crates = line |> Common.Chars |> Seq.toArray |> Array.chunkBySize 4
        for i = 0 to Array.length(crates) - 1 do
            if preStacks.Count <= i then preStacks.Add(new System.Collections.Generic.List<char>())
            let crate = crates[i][1]
            if crate <> ' ' then
                preStacks[i].Add(crate)

    let stacks = new System.Collections.Generic.List<System.Collections.Generic.Stack<char>>()

    for i = 0 to preStacks.Count - 1 do
        let preStack = preStacks[i]
        preStack.Reverse()
        let stack = new System.Collections.Generic.Stack<char>(preStack)
        stacks.Add(stack)

    let moveLines = 
        groups[1]
        |> Common.Lines

    for line in moveLines do
        let words = line |> Common.Words
        let num = (int) words[1]
        let mfr = ((int) words[3]) - 1
        let mto = ((int) words[5]) - 1
        for n = 1 to num do
            stacks[mto].Push(stacks[mfr].Pop())

    let mutable result = ""

    for stack in stacks do
        result <- result + (string)(stack.Pop())

    result
        

let Problem2 (input : string) =

    let groups = input.Replace("\n", "\r").Replace("\r\r", "\r").Split("\r\r")

    let stackLines = 
        groups[0].Replace("\n", "\r").Replace("\r\r", "\r").Split('\r') 

    let fixedStackLines = stackLines |> Array.take ((stackLines |> Array.length) - 1)

    let preStacks = new System.Collections.Generic.List<System.Collections.Generic.List<char>>()

    for line in fixedStackLines do
        let crates = line |> Common.Chars |> Seq.toArray |> Array.chunkBySize 4
        for i = 0 to Array.length(crates) - 1 do
            if preStacks.Count <= i then preStacks.Add(new System.Collections.Generic.List<char>())
            let crate = crates[i][1]
            if crate <> ' ' then
                preStacks[i].Add(crate)

    let stacks = new System.Collections.Generic.List<System.Collections.Generic.Stack<char>>()

    for i = 0 to preStacks.Count - 1 do
        let preStack = preStacks[i]
        preStack.Reverse()
        let stack = new System.Collections.Generic.Stack<char>(preStack)
        stacks.Add(stack)

    let moveLines = 
        groups[1]
        |> Common.Lines

    for line in moveLines do
        let words = line |> Common.Words
        let num = (int) words[1]
        let mfr = ((int) words[3]) - 1
        let mto = ((int) words[5]) - 1
        let q = new System.Collections.Generic.Stack<char>()
        for n = 1 to num do
            q.Push(stacks[mfr].Pop())
        for n = 1 to num do
            stacks[mto].Push(q.Pop())

    let mutable result = ""

    for stack in stacks do
        result <- result + (string)(stack.Pop())

    result
        
                

