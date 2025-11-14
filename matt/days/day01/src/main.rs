use utils;
use anyhow::Result;

fn part1(input: &str) -> Result<i32> {
    // Your solution for part 1
    todo!("Implement part 1")
}

fn part2(input: &str) -> Result<i32> {
    // Your solution for part 2
    todo!("Implement part 2")
}

fn main() -> Result<()> {
    let input = utils::read_input("day01");
    
    println!("Part 1: {}", part1(&input)?);
    println!("Part 2: {}", part2(&input)?);
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = r#"
    // Paste example input here
    "#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 0);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 0);
    }
}