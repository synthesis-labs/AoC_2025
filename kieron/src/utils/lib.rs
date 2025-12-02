use std::fs;

use nom::{
    character::complete::{char, digit1, line_ending, multispace0},
    combinator::{map_res, opt, recognize},
    multi::separated_list1,
    sequence::{delimited, pair},
    IResult,
};

/// Read input file as a string
pub fn read_input(day: &str) -> String {
    let path = format!("days/{}/input.txt", day);
    fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Failed to read input file: {}", path))
}

/// Read input file and split into lines
pub fn read_lines(day: &str) -> Vec<String> {
    read_input(day)
        .lines()
        .map(String::from)
        .collect()
}

// /// Parse lines into a specific type
// pub fn parse_lines<T>(day: &str) -> Vec<T>
// where
//     T: std::str::FromStr,
//     T::Err: std::fmt::Debug,
// {
//     read_lines(day)
//         .iter()
//         .map(|line| line.parse::<T>().unwrap())
//         .collect()
// }

pub fn parse_lines<T>(input: &str) -> Vec<T>
where 
    T: std::str::FromStr,
    T::Err: std::fmt::Debug,
{
    input
        .lines()
        // .map(String::from)
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}

pub fn parse_csv<T>(input: &str) -> Vec<T>
where 
    T: std::str::FromStr,
    T::Err: std::fmt::Debug,
{
    input
        .split(",")
        // .map(String::from)
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}



/// Convenient alias for nom results on `&str` inputs.
pub type NomResult<'a, T> = IResult<&'a str, T>;

/// Parse an unsigned integer (base 10) from the input.
pub fn unsigned(input: &str) -> NomResult<u64> {
    map_res(digit1, str::parse)(input)
}

/// Parse a signed integer (base 10) from the input.
pub fn signed(input: &str) -> NomResult<i64> {
    map_res(recognize(pair(opt(char('-')), digit1)), str::parse)(input)
}

/// Parse a comma-separated list of signed integers, tolerating surrounding whitespace.
pub fn comma_separated_ints(input: &str) -> NomResult<Vec<i64>> {
    separated_list1(
        char(','),
        delimited(multispace0, signed, multispace0),
    )(input)
}

/// Parse a newline-separated list of elements using the provided parser.
pub fn newline_separated<'a, T, F>(input: &'a str, mut parser: F) -> NomResult<'a, Vec<T>>
where
    F: FnMut(&'a str) -> NomResult<'a, T>,
{
    separated_list1(line_ending, parser)(input)
}