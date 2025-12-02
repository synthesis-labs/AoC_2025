use std::str::FromStr;

use anyhow::{anyhow, Result};
use nom::{
    bytes::complete::take_while1,
    character::complete::char as nom_char,
    combinator::{all_consuming, map},
    error::Error as nom_error,
    sequence::{preceded, tuple},
    Finish,
};
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Clone)]
struct Ranges {
    start: String,
    end: String,
}

impl FromStr for Ranges {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let trimmed = s.trim();
        let mut parser = all_consuming(tuple((
            map(take_while1(|c: char| c.is_ascii_digit()), str::to_string),
            preceded(
                nom_char('-'),
                map(take_while1(|c: char| c.is_ascii_digit()), str::to_string),
            ),
        )));

        let (_, (start, end)) = parser(trimmed)
            .finish()
            .map_err(|e: nom_error<&str>| anyhow!("Failed to parse range '{}': {}", s, e))?;

        Ok(Ranges { start, end })
    }
}

fn process_range(range: &Ranges) -> Result<i64> {
    let start = range.start.parse::<i64>()?;
    let end = range.end.parse::<i64>()?;
    let mut sum = 0;

    for i in start..=end {
        let current = i.to_string();
        // Skip odd lengths - can't be duplicates
        if current.len() % 2 != 0 {
            continue;
        }

        let mid = current.len() / 2;
        let (left, right) = current.split_at(mid);

        if left == right {
            sum += i;
        }
    }

    Ok(sum)
}

fn process_range_part2(range: &Ranges) -> Result<i64> {
    let start = range.start.parse::<i64>()?;
    let end = range.end.parse::<i64>()?;
    let mut sum = 0;

    for i in start..=end {
        let current = i.to_string();

        let mid = current.len() / 2;

        for j in 1..=mid {
            let part = &current[0..j];
            let mut found = true;
            for k in (j..current.len()).step_by(j) {                
                if k+j > current.len() {
                    if k < current.len() {
                        found = false;
                    }
                    break;
                }
                let comp = &current[k..k+j];
                if part != comp {
                    found = false;
                    break;
                }    
            }

            if found {
                sum += i;
                break;
            }
        }

    }

    Ok(sum)
}

fn part1(input: &str) -> Result<i64> {
    let ranges: Vec<Ranges> = utils::parse_csv(input);
    let total_sum = ranges.iter().try_fold(0_i64, |sum, range| {
        Ok::<i64, anyhow::Error>(sum + process_range(range)?)
    })?;
    Ok(total_sum)
}

fn part2(input: &str) -> Result<i64> {
    let ranges: Vec<Ranges> = utils::parse_csv(input);
    let total_sum = ranges.iter().try_fold(0_i64, |sum, range| {
        Ok::<i64, anyhow::Error>(sum + process_range_part2(range)?)
    })?;
    Ok(total_sum)
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
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
    "#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 1227775554);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 4174379265);
    }
}
