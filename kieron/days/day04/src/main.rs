use anyhow::{anyhow, Result};
use nom::{
    character::complete::one_of,
    combinator::{all_consuming, map_res},
    multi::many1,
};
use rayon::iter::IntoParallelRefIterator;
use std::fmt::Write;
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, PartialEq, Clone, Copy)]
enum GridCoordinate {
    Empty,
    Paper,
    Reachable,
}
impl GridCoordinate {
    fn to_char(&self) -> char {
        match self {
            GridCoordinate::Empty => '.',
            GridCoordinate::Paper => '@',
            GridCoordinate::Reachable => 'x',
        }
    }

    fn from_char(symbol: char) -> Result<GridCoordinate> {
        match symbol {
            '.' => Ok(GridCoordinate::Empty),
            '@' => Ok(GridCoordinate::Paper),
            'x' => Ok(GridCoordinate::Reachable),
            _ => return Err(anyhow!("unexpected symbol: {}", symbol)),
        }
    }
}
// ...existing code...
impl std::fmt::Display for GridCoordinate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.to_char())
    }
}
// ...existing code...
const ALL_COORDS: &[GridCoordinate] = &[
    GridCoordinate::Empty,
    GridCoordinate::Paper,
    GridCoordinate::Reachable,
];

fn parse_grid(input: &str) -> Result<Vec<Vec<GridCoordinate>>> {
    let coords_str = ALL_COORDS.iter().map(|c| c.to_char()).collect::<String>();
    let mut parser = many1::<_, _, (), _>(map_res(one_of(coords_str.as_str()), |c| {
        GridCoordinate::from_char(c)
    }));

    input
        .lines()
        .map(|l| {
            let (_, row) = parser(l)?;
            Ok(row)
        })
        .collect::<Result<Vec<Vec<GridCoordinate>>>>()
}

fn print_grid(grid: &Vec<Vec<GridCoordinate>>) {
    for row in grid {
        let line: String = row.iter().map(GridCoordinate::to_char).collect();
        println!("{line}");
    }
}

fn count_neighbours(
    grid: &Vec<Vec<GridCoordinate>>,
    row: usize,
    column: usize,
    character_to_count: GridCoordinate,
    orthoginal_only: Option<bool>,
) -> Result<u32> {
    let orthoginal = orthoginal_only.unwrap_or(false);
    const OFFSETS_4: &[(isize, isize)] = &[(-1, 0), (1, 0), (0, -1), (0, 1)];

    const OFFSETS_8: &[(isize, isize)] = &[
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    let rows = grid.len() as isize;
    let cols = grid[0].len() as isize;
    let offsets = if orthoginal { OFFSETS_4 } else { OFFSETS_8 };

    Ok(offsets
        .iter()
        .filter_map(|(dr, dc)| {
            let nr = row as isize + dr;
            let nc = column as isize + dc;
            (0 <= nr && nr < rows && 0 <= nc && nc < cols).then(|| (nr as usize, nc as usize))
        })
        .filter(|&(nr, nc)| grid[nr][nc] == character_to_count)
        .count() as u32)
}

fn part1(input: &str) -> Result<usize> {
    let original_grid = parse_grid(input)?;
    println!("Original");
    print_grid(&original_grid);
    println!("");

    let mut updated_grid = original_grid.clone();
    for (row_idx, row) in original_grid.iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            if original_grid[row_idx][col_idx] != GridCoordinate::Paper {
                continue;
            }

            let accessible = count_neighbours(
                &original_grid,
                row_idx,
                col_idx,
                GridCoordinate::Paper,
                Some(false),
            )? < 4;
            if accessible {
                updated_grid[row_idx][col_idx] = GridCoordinate::Reachable;
            }
        }
    }
    println!("Updated");
    print_grid(&updated_grid);
    println!("");

    Ok(updated_grid
        .iter()
        .flatten()
        .filter(|&&coord| coord == GridCoordinate::Reachable)
        .count())
}

fn part2(input: &str) -> Result<usize> {
    let mut original_grid = parse_grid(input)?;
    println!("Original");
    print_grid(&original_grid);
    println!("");

    let mut updated_grid = original_grid.clone();
    let mut sum = 0;
    loop {
        for (row_idx, row) in original_grid.iter().enumerate() {
            for (col_idx, _) in row.iter().enumerate() {
                if original_grid[row_idx][col_idx] != GridCoordinate::Paper {
                    continue;
                }

                let accessible = count_neighbours(
                    &original_grid,
                    row_idx,
                    col_idx,
                    GridCoordinate::Paper,
                    Some(false),
                )? < 4;
                if accessible {
                    updated_grid[row_idx][col_idx] = GridCoordinate::Reachable;
                }
            }
        }
        println!("Updated");
        print_grid(&updated_grid);
        println!("");

        let removed = updated_grid
            .iter()
            .flatten()
            .filter(|&&coord| coord == GridCoordinate::Reachable)
            .count();
        if removed == 0 {
            break;
        }
        sum += removed;

        for row in updated_grid.iter_mut() {
            for cell in row.iter_mut() {
                if *cell == GridCoordinate::Reachable {
                    *cell = GridCoordinate::Empty;
                }
            }
        }
        original_grid = updated_grid.clone();
    }

    Ok(sum)
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
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 13);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 43);
    }
}
