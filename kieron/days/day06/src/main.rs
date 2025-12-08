use anyhow::{anyhow, Result};
use nom::{
    branch::alt,
    character::complete::{i64 as nom_i64, multispace1, one_of, space0},
    combinator::{all_consuming, map, opt},
    error::Error as nom_Error,
    multi::{many1, separated_list1},
    sequence::{pair, preceded},
    Finish,
};
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Operator(char),
    Number(i64),
}

fn parse_input(input: &str) -> Result<Vec<Vec<Token>>> {
    let mut parser = many1::<_, _, nom_Error<&str>, _>(preceded(
        space0,
        alt((
            map(one_of("+-*/"), Token::Operator),
            map(nom_i64, Token::Number),
        )),
    ));

    let trimmed = input.trim();

    let grid = trimmed
        .lines()
        .map(|l| {
            let (_, row) = parser(l)
                .finish()
                .map_err(|e| anyhow!("Parse fail: {}", e))?;
            Ok(row)
        })
        .collect::<Result<Vec<Vec<_>>>>()?;

    let transposed = if grid.is_empty() {
        vec![]
    } else {
        (0..grid[0].len())
            .map(|col| grid.iter().map(|row| row[col].clone()).collect())
            .collect()
    };

    Ok(transposed)
}

fn print_grid(grid: &Vec<Vec<char>>) {
    for row in grid {
        for ch in row {
            print!("{}", ch);
        }
        println!();
    }
}

fn parse_input_part2(input: &str) -> Result<i64> {
    let grid: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();

    let transposed: Vec<Vec<char>> = if grid.is_empty() {
        vec![]
    } else {
        let max_len = grid.iter().map(|row| row.len()).max().unwrap_or(0);
        (0..max_len)
            .map(|col| {
                grid.iter()
                    .filter_map(|row| row.get(col).cloned())
                    .collect()
            })
            .collect()
    };

    print_grid(&transposed);
    println!("{:?}", transposed);

    let r: Vec<String> = transposed
        .into_iter()
        .rev()
        .map(|row| row.iter().collect())
        .collect();


    let first_value = r
        .first()
        .ok_or_else(|| anyhow!("Empty input"))?
        .trim()
        .parse::<i64>()?;

    let mut parser = all_consuming::<_, _, nom_Error<&str>, _>(preceded(
        space0,
        pair(
            map(nom_i64, Token::Number),
            opt(map(preceded(space0,one_of("+-*/")), Token::Operator)),
        ),
    ));
    let y = r
        .iter()
        .skip(1)
        .filter(|r| !r.trim().is_empty())
        .try_fold(
        (0_i64, [first_value].into()),
        |acc: (i64, Vec<i64>), row: &String| -> Result<(i64, Vec<i64>)> {

            println!("Parsing row {}", row);
            let (_, processed) = parser(&row.trim())
                .finish()
                .map_err(|e| anyhow!("Parse error: {}", e))?;

            match processed {
                (Token::Number(n), None) => {
                    let mut new_vec = acc.1.clone();
                    new_vec.push(n);
                    Ok((acc.0, new_vec))
                }
                (Token::Number(n), Some(Token::Operator(op))) => {
                    if acc.1.is_empty() {
                        return Err(anyhow!("No numbers to operate on"));
                    }
                    let mut new_vec = acc.1.clone();
                    new_vec.push(n);
                    let result =
                        new_vec
                            .iter()
                            .skip(1)
                            .try_fold(acc.1[0], |total, num| -> Result<i64> {
                                match op {
                                    '*' => Ok(total * num),
                                    '+' => Ok(total + num),
                                    '-' => Ok(total - num),
                                    '/' => Ok(total / num),
                                    _ => Err(anyhow!("Invalid operation {}", op)),
                                }
                            })?;
                    println!("Old Total {:?}, Numbers {:?}, Result {}, New Result {}", acc.0, new_vec, result, acc.0 + result);
                    Ok((acc.0 + result, Vec::new()))
                }
                _ => Err(anyhow!("Expected number, got operator")),
            }
        },
    )?;

    Ok(y.0)
}

fn process_row(input: &Vec<Token>) -> Result<i64> {
    let r = input.iter().try_fold(
        (0_i64, Vec::new()),
        |acc: (i64, Vec<i64>), t| -> Result<(i64, Vec<i64>)> {
            match t {
                Token::Number(n) => {
                    let mut new_vec = acc.1.clone();
                    new_vec.push(*n);
                    Ok((acc.0, new_vec))
                }
                Token::Operator(o) => {
                    let r = acc
                        .1
                        .iter()
                        .skip(1)
                        .try_fold(acc.1[0], |total, num| match o {
                            '*' => Ok(total * num),
                            '+' => Ok(total + num),
                            '-' => Ok(total - num),
                            '/' => Ok(total / num),
                            _ => Err(anyhow!("Invalid operation {}", o)),
                        })?;
                    Ok((acc.0 + r, Vec::new()))
                }
            }
        },
    )?;

    Ok(r.0)
}

fn part1(input: &str) -> Result<i64> {
    let parsed = parse_input(input)?;

    println!("{:?}", parsed);

    parsed.iter().map(|row| process_row(row)).sum()
}

fn part2(input: &str) -> Result<i64> {
    let parsed = parse_input_part2(input)?;

    println!("{:?}", parsed);
    Ok(parsed)
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
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 4277556);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 3263827);
    }
}
