use anyhow::{Ok, Result};
use nom::{
    character::complete::one_of,
    combinator::map_res,
    multi::many1,
};

use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

fn parse_line(input: &str) -> Result<Vec<u64>> {
    let (_, digits) = many1::<_, _, (), _>(map_res(
        one_of("0123456789"),
        |c: char| c.to_string().parse::<u64>(),
    ))(input)?;
    Ok(digits)
}

fn calculate_joltage(input: &Vec<u64>, start_index: usize, digit_number: usize) -> Result<u64> {
    // println!("Entering input {:?} start_index {} digit_number {}", input, start_index, digit_number);
    if digit_number == 0 || start_index > input.len() - digit_number {
        return Ok(0);
    }
    // println!("Past base check");
    let (index, digit) = input[start_index..input.len() - (digit_number - 1)]
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(&a.0)))
        .unwrap();

    // println!("Line: {:?}; First Digit: {} (index {}); Digit Number: {}; Start Index: {})", input, digit, index, digit_number, start_index);

    Ok(digit * 10_u64.pow((digit_number as u32) - 1) + calculate_joltage(input, index+1+start_index, digit_number-1)?)
}

fn part1(input: &str) -> Result<u64> {
    let parsed : Vec<Vec<u64>> = input
        .lines()
        .map(parse_line)        
        .collect::<Result<Vec<_>>>()?;

    parsed.iter().try_fold(0_u64, |sum, numbers| {
        let joltage = calculate_joltage(numbers, 0, 2)?;
        Ok(sum + joltage)
    })    
}

fn part2(input: &str) -> Result<u64> {
    let parsed : Vec<Vec<u64>> = input
        .lines()
        .map(parse_line)        
        .collect::<Result<Vec<_>>>()?;

    parsed.iter().try_fold(0_u64, |sum, numbers| {
        let joltage = calculate_joltage(numbers, 0, 12)?;
        Ok(sum + joltage)
    }) }

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
987654321111111
811111111111119
234234234234278
818181911112111
    "#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 357);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 3121910778619);
    }
}