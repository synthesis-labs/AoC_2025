
advent_of_code::solution!(1);

pub fn part_one(input: &str) -> Option<u64> {
    let mut cur = 50;
    let mut sum: u64 = 0;

    for c in input.lines()  {
      let (rot, uncast) = c.split_at(1);
      let val: i32 = uncast.parse().unwrap();

      cur += if rot == "L" { -val } else { val };

      if cur % 100 == 0
      {
        sum += 1;
      }
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mut cur: i32 = 50;
    let mut sum: u64 = 0;
    let mut new = 0;

    for c in input.lines()  {
      let (rot, uncast) = c.split_at(1);
      let val: i32 = uncast.parse().unwrap();
      let shift = val % 100;

      sum += (val / 100) as u64;

      if rot == "L"
      {
        new = (cur - shift + 100) % 100;
        if (new > cur || new == 0) && cur != 0
        {
            sum += 1;
        }
      }
      else if rot == "R"
      {
        new = (cur + shift) % 100;
        if new < cur
        {
            sum += 1;
        }
      }
      cur = new;
    }

    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result.is_some(), true);
    }
}
