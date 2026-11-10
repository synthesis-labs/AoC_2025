# Advent of Code 2025

# How to take part?
1. Signup for Advent of Code [here](https://adventofcode.com) (using your github account is best)
1. Join our Labs leaderboard, using join code - `2306561-242f2386` (it is [here](https://adventofcode.com/2025/leaderboard/private/view/2306561))
1. Join the Labs' AoC discord [here](https://discord.gg/2j8VSD8G2N) or Synthesis Teams [here](https://teams.microsoft.com/l/message/19:585b6ca2e2db4a5891effc453318206d@thread.skype/1762414187525?tenantId=94cfdb85-3d23-4849-a066-5cdad965ccd8&groupId=4f1b96eb-0ad7-44d2-b9e1-d47c860dab44&parentMessageId=1762414187525&teamName=Synthesis&channelName=General&createdTime=1762414187525)
1. If you would like to share your solutions please feel free to create yourself a folder (maybe with your name, e.g. "tom") and push (or PR first and we will add you as a contributor thereafter)

Any issues please reach out to Mike, Tom, Tjaard, Bhavesh, Marais, or Francois. for help!

# The Results (judged by Claude)

| Award                 | Winner       | For                                                      | Score |
|-----------------------|--------------|----------------------------------------------------------|-------|
| *The Thing of Beauty* | **Tjaard**   | [Day 6](tjaard/app/Day06.hs) in Haskell                  | 9.5   |
| *The Absolute Pits*   | **Micky**    | [Day 8](micky_gee/08/process.py) in Python               | 8.5   |
| *Eye of the Beholder* | **Vincent**  | [Day 1](vincent-de-comarmond/01/a.awk) in AWK            | 9     |
| *The Rube Goldberg*   | **Kieron**   | [Day 7](kieron/days/day07/src/main.rs) in Rust           | 9     |
| *The Ultimate Beast*  | **Francois** | 10 days in C#, with Rust ports and "fastest" rewrites    | 8.5   |

## Award 1: *The Thing of Beauty* - Tjaard

> the cleanest and most elegant solution that makes our jaws drop

The column-wise "cephalopod maths" of Day 6 part 2 cost most people forty lines of index arithmetic. Tjaard's whole parser is one pipeline:

```haskell
columns = splitWhen (all isSpace) . transpose . lines $ input
```

His [Day 1](tjaard/app/Day01.hs) is the same story: a `scanl` for the dial, and a closed-form `countZeroClicks` where everyone else wrote a pile of special cases.

Runners-up: Tom's [Day 11](tom/app/Day11.hs) (path counting as a ten-line `memoFix`, 9 / 10) and Francois's [Day 7](francois/C%23/day07.original.cs) part 2 (a bottom-up path count in eight lines, 8.5 / 10).

## Award 2: *The Absolute Pits* - Micky

> the most criminally insane solution that defies logic, but somehow works?

Day 8 wants the closest pairs of junction boxes. Micky built the full 1000 x 1000 distance matrix, flattened and sorted it, and then - having thrown away which pair each distance belonged to - went back and searched all million floats for an exact match to find out:

```python
b = np.unique(np.sort(pairwise_distances.flatten()))
connections = [np.where(pairwise_distances == x) for x in b[1:n+1]]
```

Part 2 does that search again for every single link, and after each one re-merges every circuit against every other circuit until nothing changes. It defies logic. It works. (The same folder has a [Day 7](micky_gee/07/process.py) that treats the beam splitter as a 1-D convolution over an array of path counts, which is the most physicist solution submitted.)

Dishonourable mentions:
- Francois's [Day 2, original](francois/C%23/day02.original.cs) (9 / 10, but one award each): chunks a number's digits by formatting it with a custom *thousands separator* of every possible group size and splitting on the comma. He later redeemed himself with a pure-arithmetic [fastest](francois/C%23/day02.fastest.cs) version.
- Tom's [Day 9](tom/app/Day09.hs) part 2 (8.5 / 10): drew the polygon in a window, eyeballed "the two weird points", and hard-coded their coordinates into the solution.
- Kieron's [Day 9](kieron/days/day09/src/main.rs) part 2 (8 / 10): decides a rectangle is inside the polygon by sampling every 100th point of it, in parallel.
- Liad's [Day 5](liad/haskell/src/AoC2025Day5.hs) (7.5 / 10): reinvents `any` as a `foldM` over `Either`, where `Left` means success.

## Award 3: *Eye of the Beholder* - Vincent

> the most idiomatic solution capturing the spirit of the language chosen

AWK is a language of patterns and actions, and Vincent's Day 1 is exactly that and nothing more: a `/L/` rule, an `/R/` rule, a rule that counts, and an `END`. His [Day 3](vincent-de-comarmond/03/a.awk) sets `FS = ""` to get one digit per field and builds the answer by string concatenation (`a b`). Bonus authenticity points: Day 1 initialises `num_zeros` and then counts into `num_zero`, and AWK being AWK, it still prints the right answer.

Runners-up: Micky's [Day 4](micky_gee/04/process.py) (counting neighbours with a 3x3 `convolve2d` kernel is NumPy thinking at its purest, 8.5 / 10) and Kieron's [Day 5](kieron/days/day05/src/main.rs) (a `Range` type with `merge` and `len`, `nom` parsers, `anyhow` errors and unit tests on the example - textbook Rust, 8.5 / 10).

## Award 4: *The Rube Goldberg* - Kieron

> that solution that simply had to show off

Day 7 part 2 can be done with one array and a loop. Kieron parsed the grid with `nom` into a typed token enum, built a `petgraph` DAG with a node map, topologically sorted it, ran a DP over the sort, and left a `rayon`-parallel `all_simple_paths` attempt in the file for good measure. His [Day 9](kieron/days/day09/src/main.rs) builds a complete graph of every pair of points, weighted by area, in order to find a maximum.

Runners-up: Tom's [Day 7](tom/app/Day07.hs) (a `WriterT` over `State` so the search can be replayed as a raylib animation, 9 / 10) and his [Day 10](tom/app/Day10.hs) (an SMT solver, with the comment "Claude wrote this. I have no real idea of what it's doing", 8 / 10).

## Award 5: *The Ultimate Beast* - Francois

> the individual who simply crushed it

Days 1 to 9 and Day 12 in C#, then Days 1 to 3 again in Rust, then Days 1 and 2 a third time as "fastest" rewrites because the first answer wasn't quick enough. Nobody else solved the same puzzle three times.

Runners-up: Kieron (9 days of Rust, every one with unit tests on the example, 8 / 10), Micky (8 days of NumPy, 7.5 / 10), Liad (8 days across two languages, 7.5 / 10) and Tom (all 12 days in Haskell, less the hard-coded Day 9 and the outsourced Day 10, 7.5 / 10).

## Special mentions

- **Bhavesh** - *The Polyglot*. Seven days of Haskell on his own grid and HTTP toolkit, plus a second copy in Unison. His [Day 6](bhavesh/haskell/app/AOC2025/Day06.hs) (`unsnoc`, `bimap`, `transpose`) was a near miss for Award 1.
- **Jason** - *The Pragmatist*. Opened with a README reading "this is intimidating...", then [solved Day 9 part 2](jason/python/day9/day09.py) by handing the polygon to `shapely`. Knowing which library to reach for is a skill.
- **Jeanette** - *The Narrator*. Five days of C# with the most thoroughly explained console output of the year - you can follow every dial turn. [Day 5](jeanette/c%23/aoc_2025/Days/Day5/Day5.cs)'s range merge is tidy, and Day 4 packs each coordinate into a single `int`.
- **Liad** - *The Explainer*. Seven days of Haskell with a one-day holiday in Java for Day 7. The comments walking through [Day 3](liad/haskell/src/AoC2025Day3.hs)'s greedy choice are the best explanation of that puzzle in the repo.
- **Matt** - *Best Prepared*. Cargo workspace, day template and `new_day.sh` all in place. 2026 is the year.
- **Miguel** - *The Folder*. Five days of Haskell with Parsec; [Day 5](miguel/haskell/day5.hs) part 2 is a single `foldl'` over the sorted ranges.
- **Rui** - *The Pipeliner*. [Day 1](rui/f%23/day01/part01/Main.fsx) in F# as a `Seq.fold` pipeline, with a debug log of every dial step and a README documenting the approach.
- **Tom** - *The Completionist*. All 12 days in Haskell, with a parser combinator for every input.

# Previous years

| Year |                                                                               |                                                           |
|------|-------------------------------------------------------------------------------|-----------------------------------------------------------|
| 2022 | [leaderboard](https://adventofcode.com/2022/leaderboard/private/view/2306561) | [github repo](https://github.com/synthesis-labs/AoC_2022) |
| 2023 | [leaderboard](https://adventofcode.com/2023/leaderboard/private/view/2306561) | [github repo](https://github.com/synthesis-labs/AoC_2023) |
| 2024 | [leaderboard](https://adventofcode.com/2024/leaderboard/private/view/2306561) | [github repo](https://github.com/synthesis-labs/AoC_2024) |
