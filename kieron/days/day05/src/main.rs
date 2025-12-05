use std::collections::HashSet;

use anyhow::{Result, anyhow};
use nom::{
    Finish, 
    character::complete::{char as nom_char, i64 as nom_i64, multispace1}, 
    combinator::{all_consuming, map}, 
    error::Error as nom_Error, 
    multi::separated_list1, 
    sequence::{pair, preceded}
};
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Clone, Copy)]
struct Range {
    start: i64,
    end: i64,
}

impl Range {
    fn overlaps_or_adjacent(&self, other: &Range) -> bool {
        self.start <= other.end + 1 && other.start <= self.end + 1
    }

    fn merge(&self, other: &Range) -> Range {
        Range {
            start: self.start.min(other.start),
            end: self.end.max(other.end)
        }
    }

    fn len(&self) -> i64 {
        self.end - self.start + 1
    }
}

fn parse_fresh(input: &str) -> Result<Vec<Range>> {
    let mut parser = all_consuming(separated_list1(multispace1,map(
        pair(nom_i64, 
            preceded(nom_char('-'), nom_i64)),
            |(start, end)| Range { start, end }
        )));
    let trimmed = input.trim();

    let (_, result) = parser(trimmed).finish().map_err(|e: nom_Error<&str>| anyhow!("Failed parsing range '{}': {}", trimmed, e))?;

    Ok(result)
}

fn parse_available(input: &str) -> Result<Vec<i64>> {
    let mut parser = all_consuming(separated_list1(multispace1, nom_i64));
    let trimmed = input.trim();

    let (_, result) = parser(trimmed).finish().map_err(|e: nom_Error<&str>| anyhow!("Failed parsing range '{}': {}", trimmed, e))?;

    Ok(result)
}

fn test_if_fresh(ingredient: i64, fresh: &Vec<Range>) -> bool {
    fresh.iter().any(|f| {
        ingredient >= f.start && ingredient <= f.end
    })
}

fn part1(input: &str) -> Result<usize> {
    let (fresh, available) = input.split_once("\n\n").ok_or_else(|| anyhow!("Invalid input format"))?;

    let parsed_fresh = parse_fresh(fresh)?;
    let parsed_available = parse_available(available)?;

    println!("Fresh:{:?}; Available:{:?}", parsed_fresh, parsed_available);

    Ok(parsed_available
        .iter()
        .filter(|ingredient| test_if_fresh(**ingredient, &parsed_fresh))        
        .count())
}

fn merge_ranges(mut ranges: Vec<Range>) -> Vec<Range> {
    if ranges.is_empty() {
        return vec![];
    }

    // Sort by start position
    ranges.sort_by_key(|r| r.start);

    let mut merged = Vec::new();
    let mut current = ranges[0].clone();

    for next in ranges.into_iter().skip(1) {
        if current.overlaps_or_adjacent(&next) {
            current = current.merge(&next);
        } else {
            merged.push(current);
            current = next;
        }
    }
    merged.push(current);
    merged
}

fn part2(input: &str) -> Result<i64> {
    let (fresh, _) = input.split_once("\n\n").ok_or_else(|| anyhow!("Invalid input format"))?;

    let parsed_fresh = parse_fresh(fresh)?;
    let merged = merge_ranges(parsed_fresh);

    Ok(merged.iter().map(|r| r.len()).sum())   
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
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 3);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 14);
    }
}