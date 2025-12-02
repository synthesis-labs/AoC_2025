advent_of_code::solution!(2);

pub fn part_one(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let vals = input.split(',');
    for c in vals  {
      let mut parts = c.split('-');
      let start: i64 = parts.next().unwrap().parse().unwrap();
      let end: i64 = parts.next().unwrap().parse().unwrap();

      for cur in start..=end {
        let digit = cur.ilog10() + 1;
        if digit % 2 != 0 {
            continue;
        }

        let div = 10_i64.pow(digit / 2);
        if cur / div == cur % div
        {
            sum += cur as u64;
        }
      }
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let vals = input.split(',');
    for c in vals  {
      let mut parts = c.split('-');
      let start: i64 = parts.next().unwrap().parse().unwrap();
      let end: i64 = parts.next().unwrap().parse().unwrap();

      for cur in start..=end {
        let digit = cur.ilog10() + 1;

        for patt in 1..=(digit / 2) {
            if digit % patt != 0 {
                continue;
            }

            let pattern = cur / 10_i64.pow(digit - patt as u32);
            let div = 10_i64.pow(patt as u32) - 1;
            let mul = 10_i64.pow(digit as u32) - 1;

            if cur * div == pattern * mul
            {
                sum += cur as u64;
                break;
            }
        }
      }
    }

    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(1227775554));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(4174379265));
    }
}
