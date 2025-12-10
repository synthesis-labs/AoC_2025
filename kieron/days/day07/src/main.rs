use std::{collections::{HashMap}};

use anyhow::{anyhow, Result};
use nom::{
    character::complete::one_of,
    combinator::{all_consuming, map_res},
    multi::many1,
};
use rayon::prelude::*;
use utils;
use petgraph::{
    Direction, algo::{all_simple_paths, toposort}, graph::{Graph, NodeIndex}, visit::NodeRef
};

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, PartialEq, Clone, Copy)]
enum GridToken {
    Start,
    Empty,
    Splitter,
    Tachyon,
}

impl GridToken {
    fn from_char(c: char) -> Result<GridToken> {
        match c {
            'S' => Ok(GridToken::Start),
            '.' => Ok(GridToken::Empty),
            '^' => Ok(GridToken::Splitter),
            '|' => Ok(GridToken::Tachyon),
            _ => return Err(anyhow!("unexpected symbol: {}", c)),
        }
    }

    fn to_char(&self) -> char {
        match self {
            GridToken::Empty => '.',
            GridToken::Splitter => '^',
            GridToken::Start => 'S',
            GridToken::Tachyon => '|',
        }
    }
}

fn parse_input(input: &str) -> Result<Vec<Vec<GridToken>>> {
    let mut parser = many1::<_, _, (), _>(map_res(one_of(".S^|"), |c| GridToken::from_char(c)));

    input
        .trim()
        .lines()
        .map(|l| {
            let (_, row) = parser(l)?;
            Ok(row)
        })
        .collect::<Result<Vec<Vec<GridToken>>>>()
}

fn print_grid(grid: &Vec<Vec<GridToken>>) {
    for row in grid {
        let line: String = row.iter().map(GridToken::to_char).collect();
        println!("{line}");
    }
}

fn progress_grid(
    grid: &Vec<Vec<GridToken>>,
    row_num: usize,
) -> Result<(Vec<Vec<GridToken>>, usize)> {
    let mut new_grid = grid.clone();
    let mut split_count = 0;
    for (col, token) in grid[row_num].iter().enumerate() {
        match grid[row_num - 1][col] {
            GridToken::Start | GridToken::Tachyon => {
                if *token != GridToken::Splitter {
                    new_grid[row_num][col] = GridToken::Tachyon;
                } else {
                    new_grid[row_num][col - 1] = GridToken::Tachyon;
                    new_grid[row_num][col + 1] = GridToken::Tachyon;
                    split_count += 1;
                }
            }
            GridToken::Splitter => {
                // new_grid[row_num][col - 1] = GridToken::Tachyon;
                // new_grid[row_num][col + 1] = GridToken::Tachyon;
            }
            _ => {}
        }
    }

    Ok((new_grid, split_count))
}

fn part1(input: &str) -> Result<usize> {
    // Your solution for part 1
    let grid = parse_input(input)?;
    // print_grid(&grid);

    let mut updated = grid.clone();

    (1..grid.len()).try_fold(0_usize, |acc: usize, row_idx| -> Result<usize> {
        let split_count;
        (updated, split_count) = progress_grid(&updated, row_idx)?;
        // println!("Split count: {}. Total: {}", split_count, acc + split_count);
        println!();
        // print_grid(&updated);
        Ok(acc + split_count)
    })
}

fn parse_input_as_dag(input: &str) -> Result<Graph<(usize, usize), ()>> {
    let grid = parse_input(&input)?;

    let start_column = grid[0].iter().position(|r| *r == GridToken::Start).unwrap();

    let mut graph : Graph<(usize, usize), ()> = Graph::new();
    let mut node_map: HashMap<(usize, usize), NodeIndex> = HashMap::new();

    let start_node = graph.add_node((0_usize, start_column));

    let mut leaves = vec![start_node];
    node_map.insert((0_usize, start_column), start_node);

    for row_idx in (1..grid.len()) {
        let mut new_leaves = Vec::new();
        for leaf in &leaves {
            let &(_, col) = graph.node_weight(*leaf).unwrap();
            match grid[row_idx][col] {
                GridToken::Splitter => {
                    // Find existing
                    match node_map.get(&(row_idx, col-1)) {
                        Some(&node_idx) => {
                            graph.add_edge(*leaf, node_idx, ());
                        }
                        None => {
                            let l = graph.add_node((row_idx, col-1));
                            graph.add_edge(*leaf, l, ());
                            new_leaves.push(l);
                            node_map.insert((row_idx, col-1), l);
                        }

                    }

                    match node_map.get(&(row_idx, col+1)) {
                        Some(&node_idx) => {
                            graph.add_edge(*leaf, node_idx, ());
                        }
                        None => {
                            let r = graph.add_node((row_idx, col+1));
                            graph.add_edge(*leaf, r, ());
                            new_leaves.push(r);
                            node_map.insert((row_idx, col+1), r);
                        }
                    }                    
                }
                _ => {
                    new_leaves.push(*leaf);
                }
            }
        }
        leaves = new_leaves
    }

    Ok(graph)
}


fn print_dag(grid: &Graph<(usize, usize), ()>) {
        let max_row = grid.node_indices()
        .filter_map(|idx| grid.node_weight(idx).map(|&(r, _)| r))
        .max()
        .unwrap_or(0);
    
    let max_col = grid.node_indices()
        .filter_map(|idx| grid.node_weight(idx).map(|&(_, c)| c))
        .max()
        .unwrap_or(0);

for row in 0..=max_row {
        for col in 0..=max_col {
            // Find node at this position
            let node_exists = grid.node_indices()
                .find(|&idx| {
                    grid.node_weight(idx) == Some(&(row, col))
                });
            
            match node_exists {
                Some(node_idx) => {
                    // Check how many children this node has
                    let child_count = grid.neighbors_directed(node_idx, Direction::Outgoing).count();
                    if child_count > 1 {
                        print!("^");  // Splitter
                    } else if row == 0 {
                        print!("S");  // Start
                    } else {
                        print!(".");  // Tachyon path
                    }
                },
                None => print!("."),  // Empty
            }
        }
        println!();
    }
    
}

fn count_paths_dp(graph: &Graph<(usize, usize), ()>) -> Result<usize> {
    let start = graph.node_indices()
        .find(|&idx| graph.node_weight(idx).map(|&(r, _)| r == 0).unwrap_or(false))
        .expect("No start node");
    
    // Topological sort ensures we process nodes in order
    let topo_order = toposort(&graph, None).map_err(|_| anyhow!("Graph has cycle"))?;
    
    let mut path_counts: HashMap<NodeIndex, usize> = HashMap::new();
    path_counts.insert(start, 1);
    
    for node in topo_order {
        let count = *path_counts.get(&node).unwrap_or(&0);
        if count > 0 {
            for neighbor in graph.neighbors_directed(node, Direction::Outgoing) {
                *path_counts.entry(neighbor).or_insert(0) += count;
            }
        }
    }
    
    // Sum paths to all leaf nodes
    Ok(graph.node_indices()
        .filter(|&idx| graph.neighbors_directed(idx, Direction::Outgoing).count() == 0)
        .map(|idx| path_counts.get(&idx).unwrap_or(&0))
        .sum())
}

fn count_paths(graph: &Graph<(usize, usize), ()>) -> usize {
    // Find start node (row == 0)
    let start = graph.node_indices()
        .find(|&idx| {
            graph.node_weight(idx).map(|&(r, _)| r == 0).unwrap_or(false)
        })
        .expect("No start node found");

    println!("Start: {:?}", start);
    
    // Find all end nodes (nodes with no outgoing edges)
    let ends: Vec<_> = graph.node_indices()
        .filter(|&idx| {
            graph.neighbors_directed(idx, Direction::Outgoing).count() == 0
        })
        .collect();
    println!("Ends: {:?}", ends);
    
    ends.par_iter()
        .map(|&end| {
            all_simple_paths::<Vec<_>, _>(&graph, start, end, 0, None).count()
        })
        .sum()

    // Count paths from start to each end
    // let mut total_paths = 0;
    // for end in ends {
    //     let paths = all_simple_paths::<Vec<_>, _>(&graph, start, end, 0, None);
    //     total_paths += paths.count();
    //     println!("Total paths {}", total_paths);
    // }
    
    // total_paths
}

fn part2(input: &str) -> Result<usize> {
    let graph = parse_input_as_dag(&input)?;

    print_dag(&graph);

    

    count_paths_dp(&graph)
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
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 21);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 40);
    }
}
