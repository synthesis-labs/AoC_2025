# Rui's Advent of Code 2025 Solutions

## Language: F#

Solutions for Advent of Code 2025 written in F# script files.

## Structure

```
f#/
  day01/
    input.txt
    part01/
      Main.fsx
      debug_output.txt
```

## Running Solutions

### Prerequisites
- .NET SDK (with F# support)

### Run a Solution

Navigate to the solution directory and run:

```bash
cd f#/day01/part01
dotnet fsi Main.fsx
```

## Solutions Completed

### Day 1: Secret Entrance ⭐
- **Part 1**: Circular dial password counter
- **Approach**: Uses `Seq.fold` to track dial rotations and count zeros
- **Key concepts**: Modulo arithmetic for circular wrapping, functional pipeline with state accumulation

#### Implementation Highlights
- `processDial`: Handles left/right rotations with circular wrap-around (0-99)
- `countZeros`: Accumulates count when dial points to 0
- Debug output written to file for analysis of each step
