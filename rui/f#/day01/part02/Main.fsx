// Day 01 - Part 01

let readFileLine (filename: string) =
    seq {
        use reader = new System.IO.StreamReader(filename)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let splitStringToCharInt (input: string) =
    let charPart = input[0]
    let intPart = input[1..] |> int
    charPart, intPart

let processDial (dialValue: int) (direction: char, steps: int) =
    let newDialValue = 
        match direction with
        | 'R' -> dialValue + steps
        | 'L' -> dialValue - steps
        | _ -> failwithf "Invalid direction: %c" direction
    
    let wrapped = newDialValue % 100
    if wrapped < 0 then wrapped + 100 else wrapped

let countZeroRotations (oldValue: int) (direction: char) (steps: int) (counter: int) =
    // Count how many times we pass through or land on zero during the rotation
    // Reuse the same wrapping logic as processDial for each step to count landing zeros
    let zerosHit = 
        [1 .. steps]
        |> List.filter (fun i ->
            let position = processDial oldValue (direction, i)
            position = 0
        )
        |> List.length
    
    counter + zerosHit

let inputPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt")
let debugOutputPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "debug_output.txt")
let startDialAt = 50

let totalZeros = 
    use debugWriter = new System.IO.StreamWriter(debugOutputPath)
    
    let result = 
        readFileLine inputPath 
        |> Seq.map splitStringToCharInt
        |> Seq.fold (fun (dialValue, counter) (direction, steps) -> 
            let newDialValue = processDial dialValue (direction, steps)
            let newCounter = countZeroRotations dialValue direction steps counter
            let debugLine = sprintf "Instruction: %c%d, DialValue: %d -> %d, Counter: %d" direction steps dialValue newDialValue newCounter
            debugWriter.WriteLine debugLine
            newDialValue, newCounter
        ) (startDialAt, 0)
        |> snd
    
    result

printfn "Total zeros: %d" totalZeros
printfn "Debug output written to: %s" debugOutputPath
