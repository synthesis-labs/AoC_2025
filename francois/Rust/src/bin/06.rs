use std::collections::HashMap;

advent_of_code::solution!(6);

pub fn part_one(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let mut valuemap: HashMap<usize, Vec<u64>> = HashMap::new();
    let mut operandmap: HashMap<usize, String> = HashMap::new();

    for line in input.lines()
    {
        let splits: Vec<&str> = line.split_whitespace().collect();
        for (x, &part) in splits.iter().enumerate()
        {
            if let Ok(value) = part.parse::<u64>()
            {
                valuemap.entry(x).or_default().push(value);
            }
            else
            {
                operandmap.insert(x, part.to_string());
            }
        }
    }

    let keys_count = valuemap.len();
    for x in 0..keys_count
    {
        match operandmap.get(&x).map(String::as_str)
        {
            Some("+") =>
            {
                if let Some(values) = valuemap.get(&x)
                {
                    sum += values.iter().sum::<u64>();
                }
            }
            Some("*") => {
                if let Some(values) = valuemap.get(&x)
                {
                    let product = values.iter().product::<u64>();
                    sum += product;
                }
            }
            _ => {}
        }
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mut sum: u64 = 0;
    let mut valuemap: HashMap<usize, Vec<String>> = HashMap::new();
    let operations = input.lines().last().unwrap();
    let mut operation_indexes: Vec<usize> = Vec::new();

    for (x, ch) in operations.chars().enumerate()
    {
        if ch != ' '
        {
            operation_indexes.push(x);
        }
    }

    for (x, &operation_index) in operation_indexes.iter().enumerate()
    {
        let next_ind = if x + 1 < operation_indexes.len()
        {
            operation_indexes[x + 1]
        }
        else
        {
            operations.len()
        };
        let mut current_problem: Vec<String> = Vec::new();
        for line in input.lines()
        {
            current_problem.push(line[operation_index..(next_ind)].to_string());
        }
        valuemap.insert(x, current_problem);
    }

    let keys_count = valuemap.len();
    for x in 0..keys_count
    {
        let cur_operation = valuemap
            .get(&x)
            .and_then(|v| v.last())
            .map(|s| s.trim())
            .unwrap_or("");
        let mut variables: Vec<u64> = Vec::new();

        if let Some(first_line) = valuemap.get(&x).and_then(|v| v.first())
        {
            let len = first_line.len();
            for y in 0..len
            {
                let mut cur_var = String::new();
                if let Some(lines) = valuemap.get(&x)
                {
                    for z in 0..lines.len() - 1 {
                        let line = &lines[z];
                        if y < line.len() {
                            cur_var.push(line.chars().nth(y).unwrap());
                        }
                    }
                }
                if let Ok(value) = cur_var.to_string().replace(" ", "").parse::<u64>()
                {
                    variables.push(value);
                }
            }
        }

        match cur_operation
        {
            "+" =>
            {
                sum += variables.iter().sum::<u64>();
            }
            "*" =>
            {
                let product = variables.iter().product::<u64>();
                sum += product;
            }
            _ => {}
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
        assert_eq!(result, Some(4277556));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(3263827));
    }
}
