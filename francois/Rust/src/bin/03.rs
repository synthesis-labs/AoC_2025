advent_of_code::solution!(3);

pub fn part_one(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;

    for line in input.lines()
    {
        let int_list: Vec<i32> = line.chars().map(|q| q as i32 - '0' as i32).collect();
        let mut active = 0;
        let mut surplus = 1;
        let mut cur_max: String = "".to_owned();
        let mut cur_bank = int_list.clone();

        while active < 2 {
            if surplus == 0
            {
                if let Some(max_val) = cur_bank.iter().max()
                {
                    cur_max.push_str(&max_val.to_string());
                    break;
                }
            }

            let valid_max = *cur_bank.iter().take(cur_bank.len().saturating_sub(surplus)).max().unwrap();
            let valid_max_pos = cur_bank.iter().position(|&cur| cur == valid_max).unwrap() + 1;

            cur_max.push_str(&valid_max.to_string());
            surplus -= 1;
            active += 1;
            cur_bank = cur_bank.into_iter().skip(valid_max_pos).collect();
        }

        sum += cur_max.parse::<u64>().unwrap();
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;

    for line in input.lines()
    {
        let int_list: Vec<i32> = line.chars().map(|q| q as i32 - '0' as i32).collect();
        let mut active = 0;
        let mut surplus = 11;
        let mut cur_max: String = "".to_owned();
        let mut cur_bank = int_list.clone();

        while active < 12 {
            if surplus == 0
            {
                if let Some(max_val) = cur_bank.iter().max()
                {
                    cur_max.push_str(&max_val.to_string());
                    break;
                }
            }

            let valid_max = *cur_bank.iter().take(cur_bank.len().saturating_sub(surplus)).max().unwrap();
            let valid_max_pos = cur_bank.iter().position(|&cur| cur == valid_max).unwrap() + 1;

            cur_max.push_str(&valid_max.to_string());
            surplus -= 1;
            active += 1;
            cur_bank = cur_bank.into_iter().skip(valid_max_pos).collect();
        }

        sum += cur_max.parse::<u64>().unwrap();
    }

    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(357));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(3121910778619));
    }
}
