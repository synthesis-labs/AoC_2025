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

let countZeros (value: int) (counter: int) =
    if value = 0 then counter + 1 else counter

// Get the input file path relative to the script's directory
let inputPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "input.txt")
let debugOutputPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "debug_output.txt")
let startDialAt = 50

let totalZeros = 
    use debugWriter = new System.IO.StreamWriter(debugOutputPath)
    
    let result = 
        readFileLine inputPath 
        |> Seq.map splitStringToCharInt
        |> Seq.fold (fun (dialValue, counter) instruction -> 
            let newDialValue = processDial dialValue instruction
            let newCounter = countZeros newDialValue counter
            let debugLine = sprintf "Instruction: %A, DialValue: %d -> %d, Counter: %d" instruction dialValue newDialValue newCounter
            debugWriter.WriteLine debugLine
            newDialValue, newCounter
        ) (startDialAt, 0)
        |> snd
    
    result

printfn "Total zeros: %d" totalZeros
printfn "Debug output written to: %s" debugOutputPath
