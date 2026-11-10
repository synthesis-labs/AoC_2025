advent_of_code::solution!(4);

pub fn part_one(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    for x in 0..map.len()
    {
        for y in 0..map[x].len()
        {
            let mut num_adj = 0;
            if map[x][y] == '@'
            {
                if x > 0
                {
                    if map[x-1][y] == '@'
                    {
                        num_adj += 1;
                    }

                    if y > 0 && map[x - 1][y - 1] == '@'
                    {
                        num_adj += 1;
                    }

                    if y < map[x].len() - 1 && map[x - 1][y + 1] == '@'
                    {
                        num_adj += 1;
                    }
                }

                if y > 0 && map[x][y - 1] == '@'
                {
                    num_adj += 1;
                }

                if x < map.len() - 1
                {
                    if map[x + 1][y] == '@'
                    {
                        num_adj += 1;
                    }

                    if y > 0 && map[x + 1][y - 1] == '@'
                    {
                        num_adj += 1;
                    }
                }
                if y < map[x].len() - 1 && map[x][y + 1] == '@'
                {
                    num_adj += 1;
                }

                if y < map[x].len() - 1 && x < map.len() - 1 && map[x + 1][y + 1] == '@'
                {
                    num_adj += 1;
                }

                if num_adj < 4
                {
                    sum += 1;
                }
            }
        }
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let mut map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    let mut x = 0;
    while x < map.len()
    {
        let mut y = 0;
        while y < map[x].len()
        {
            let mut num_adj = 0;
            if map[x][y] == '@'
            {
                if x > 0
                {
                    if map[x-1][y] == '@'
                    {
                        num_adj += 1;
                    }

                    if y > 0 && map[x - 1][y - 1] == '@'
                    {
                        num_adj += 1;
                    }

                    if y < map[x].len() - 1 && map[x - 1][y + 1] == '@'
                    {
                        num_adj += 1;
                    }
                }

                if y > 0 && map[x][y - 1] == '@'
                {
                    num_adj += 1;
                }

                if x < map.len() - 1
                {
                    if map[x + 1][y] == '@'
                    {
                        num_adj += 1;
                    }

                    if y > 0 && map[x + 1][y - 1] == '@'
                    {
                        num_adj += 1;
                    }
                }
                if y < map[x].len() - 1 && map[x][y + 1] == '@'
                {
                    num_adj += 1;
                }

                if y < map[x].len() - 1 && x < map.len() - 1 && map[x + 1][y + 1] == '@'
                {
                    num_adj += 1;
                }

                if num_adj < 4
                {
                    sum += 1;
                    map[x][y] = '.';
                    x = 0;
                    y = 0;
                }
                else
                {
                    y += 1;
                }
            }
            else
            {
                y += 1;
            }
        }
        x += 1;
    }

    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(13));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(43));
    }
}
