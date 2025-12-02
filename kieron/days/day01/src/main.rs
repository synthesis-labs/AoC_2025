use anyhow::{anyhow, Result};
use nom::{
    character::complete::{i32 as nom_i32, one_of},
    combinator::all_consuming,
    sequence::pair,
    Finish,
    error::Error as nom_Error
};
use std::{cmp::{max, min}, str::FromStr};
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");
const MAX_STEPS: i32 = 100;

#[derive(Debug, Copy, Clone)]
enum Direction {
    Left=-1,
    Right=1
}

#[derive(Debug, Copy, Clone)]
struct Instruction {
    direction: Direction,
    steps: i32,
}

impl FromStr for Instruction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let trimmed = s.trim();
        let mut parser = all_consuming(pair(one_of("LR"), nom_i32));

        let (_, (direction, steps)) = parser(trimmed)
            .finish()
            .map_err(|e: nom_Error<&str>| anyhow!("failed to parse instruction '{}': {}", s, e))?;

        let direction = match direction {
            'L' => Direction::Left,
            'R' => Direction::Right,
            _ => return Err(anyhow!("invalid direction {}", direction))
        };

        Ok(Instruction { direction, steps })
    }
}

fn process_line(instruction: Instruction, start_num: i32, count_middle: bool) -> Result<(i32, i32)> {

    let mut pos = start_num;
    let mut steps = instruction.steps;
    let direction = instruction.direction as i32;
    let mut zero_hits = 0;
    while steps > 0 {
        let next_increment = min(MAX_STEPS, steps);
        steps -= next_increment;
        let new_pos = pos + (next_increment * direction);
        if count_middle {
            if (new_pos < 0 || new_pos > MAX_STEPS) && (pos != 0 || steps > 0) {
                zero_hits += 1;
            }
        } 
        if new_pos == 0 || new_pos == MAX_STEPS {
            zero_hits += 1;
        }
        pos = (new_pos + MAX_STEPS) % MAX_STEPS;
    }

    Ok((pos, zero_hits))
}

fn part1(input: &str) -> Result<i32> {
    let parsed : Vec<Instruction> = utils::parse_lines(input);

    let (_i32, zero_hits) = parsed
        .into_iter()
        .try_fold((50_i32, 0_i32), |(pos, hits), instruction| {
            let (new_pos, new_hits) = process_line(instruction, pos, false)?;
            // let new_hits = hits + if new_pos == 0 { 1 } else { 0 };
            Ok::<(i32, i32), anyhow::Error>((new_pos, new_hits + hits))
        })?;

    Ok(zero_hits)
}

fn part2(input: &str) -> Result<i32> {
    let parsed : Vec<Instruction> = utils::parse_lines(input);

    let (_i32, zero_hits) = parsed
        .into_iter()
        .try_fold((50_i32, 0_i32), |(pos, hits), instruction| {
            let (new_pos, new_hits) = process_line(instruction, pos, true)?;
            Ok::<(i32, i32), anyhow::Error>((new_pos, new_hits + hits))
        })?;

    Ok(zero_hits)
}

fn main() -> Result<()> {
    let input = utils::read_input(DAY);

    println!("Part 1: {}", part1(&input)?);
    println!("Part 2: {}", part2(&input)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = r#"
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
    "#;

    #[test]
    fn test_part1() {
        println!("Test {}", TEST_INPUT);
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 3);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 6);
    }
}
