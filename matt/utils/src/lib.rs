use std::fs;
use std::path::Path;

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

/// Parse lines into a specific type
pub fn parse_lines<T>(day: &str) -> Vec<T>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Debug,
{
    read_lines(day)
        .iter()
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}