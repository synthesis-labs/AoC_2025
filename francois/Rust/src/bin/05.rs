use std::{cmp::max, str::FromStr};
advent_of_code::solution!(5);

pub fn part_one(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let mut fresh_ranges: Vec<(i64, i64)> = Vec::new();
    for line in input.lines()
    {
        let split: Vec<&str> = line.split('-').collect();
        if split.len() == 2
        {
            if let (Ok(start), Ok(end)) = (i64::from_str(split[0]), i64::from_str(split[1]))
            {
               fresh_ranges.push((start, end));
            }
        }
        else
        {
            if let Ok(checkval) = i64::from_str(split[0])
            {
                if checkval > 0
                {
                    if fresh_ranges.iter().any(|&(start, end)| checkval >= start && checkval <= end)
                    {
                        sum += 1;
                    }
                }
            }
        }
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let sum: u64;
    let mut fresh_ranges: Vec<(i64, i64)> = Vec::new();
    let mut full_ranges: Vec<(i64, i64)> = Vec::new();
    for line in input.lines()
    {
        let split: Vec<&str> = line.split('-').collect();
        if split.len() == 2
        {
            if let (Ok(start), Ok(end)) = (i64::from_str(split[0]), i64::from_str(split[1]))
            {
               fresh_ranges.push((start, end));
            }
        }
    }
    fresh_ranges.sort_by_key(|x| x.0);

    for range in fresh_ranges
    {
        if full_ranges.is_empty()
        {
            full_ranges.push(range);
        }
        else
        {
            let prev = full_ranges.last_mut().unwrap();
            if range.0 <= prev.1
            {
                prev.1 = max(prev.1, range.1);
            }
            else
            {
                full_ranges.push(range);
            }
        }
    }

    sum = full_ranges.iter().map(|x| x.1 as u64 - x.0 as u64 + 1).sum();
    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(3));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(14));
    }
}
