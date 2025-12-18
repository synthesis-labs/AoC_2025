use std::cmp::{max, min};

use anyhow::{Result, anyhow};
use nom::{
    character::complete::{u64 as nom_u64, char as nom_char}, 
    multi::{many1, separated_list1}
};
use petgraph::{Graph, visit::EdgeRef};
use utils;
use rayon::prelude::*;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Coordinate {
    col: u64,
    row: u64,
}

fn calculate_area(a: &Coordinate, b: &Coordinate) -> u64 {
    let top_left = Coordinate {
        col: min(a.col, b.col), 
        row: min(a.row,b.row)
    };
    let bottom_right = Coordinate {
        col: max(a.col, b.col), 
        row: max(a.row, b.row)
    };

    (bottom_right.col - top_left.col + 1) * (bottom_right.row - top_left.row + 1)
}

fn parse_coords(input: &str) -> Result<Graph<Coordinate, u64>> {
    let mut parser = many1::<_,_,(),_>(
        separated_list1(nom_char(','), nom_u64)
    );

    let coords: Vec<Coordinate> = input
        .trim()
        .lines()
        .filter_map(|l| parser(l).ok())
        .flat_map(|(_, vecs)| vecs)
        .map(|c| Coordinate { col: c[0], row: c[1], })
        .collect();

    let mut graph = Graph::new();
    
    let nodes: Vec<_> = coords.into_iter()
        .map(|coord| graph.add_node(coord))
        .collect();
    
    for i in 0..nodes.len() {
        for j in (i + 1)..nodes.len() {
            let area = calculate_area(
                graph.node_weight(nodes[i]).unwrap(),
                graph.node_weight(nodes[j]).unwrap()
            );
            graph.add_edge(nodes[i], nodes[j], area);
        }
    }
    
    Ok(graph)

}

fn part1(input: &str) -> Result<u64> {
    let coords = parse_coords(input)?;

    let edge = coords.edge_references()
        .max_by(|a, b| a.weight().cmp(b.weight()))
        .unwrap();

    let area = *edge.weight();
    let source = coords.node_weight(edge.source()).unwrap();
    let target = coords.node_weight(edge.target()).unwrap();
    // Your solution for part 1
    Ok(area)
}

fn point_in_polygon(point: &Coordinate, polygon: &[Coordinate]) -> bool {
    let mut inside = false;
    let n = polygon.len();
    
    for i in 0..n {
        let j = (i + 1) % n;
        let vi = &polygon[i];
        let vj = &polygon[j];
        
        // Check if point is on the edge (including endpoints)
        if point_on_segment(point, vi, vj) {
            return true;
        }
        
        // Skip horizontal edges for ray casting
        if vi.row == vj.row {
            continue;
        }
        
        // Only count edges where one endpoint is strictly above and one is at or below
        let yi_above = vi.row > point.row;
        let yj_above = vj.row > point.row;
        
        if yi_above != yj_above {
            // Calculate intersection x-coordinate
            let x_intersect = (vj.col as i64 - vi.col as i64) * (point.row as i64 - vi.row as i64) 
                            / (vj.row as i64 - vi.row as i64) + vi.col as i64;
            
            if point.col < x_intersect as u64 {
                inside = !inside;
            }
        }
    }
    
    inside
}

fn point_on_segment(point: &Coordinate, a: &Coordinate, b: &Coordinate) -> bool {
    // Check if point is collinear and within the bounding box of the segment
    let cross_product = (point.row as i64 - a.row as i64) * (b.col as i64 - a.col as i64) 
                      - (point.col as i64 - a.col as i64) * (b.row as i64 - a.row as i64);
    
    if cross_product != 0 {
        return false;
    }
    
    // Check if point is within segment bounds
    point.col >= min(a.col, b.col) && point.col <= max(a.col, b.col) &&
    point.row >= min(a.row, b.row) && point.row <= max(a.row, b.row)
}

fn rectangle_in_polygon(top_left: &Coordinate, bottom_right: &Coordinate, polygon: &[Coordinate]) -> bool {
    // Check corners
    let corners = [
        *top_left,
        Coordinate { col: bottom_right.col, row: top_left.row },
        *bottom_right,
        Coordinate { col: top_left.col, row: bottom_right.row },
    ];
    
    for corner in &corners {
        if !point_in_polygon(corner, polygon) {
            return false;
        }
    }
    
    /// Sample points inside the rectangle in parallel
    let step_size = 100;
    let rows: Vec<u64> = (top_left.row..=bottom_right.row)
        .step_by(step_size as usize)
        .collect();

    rows.into_par_iter()
        .all(|row| {
            (top_left.col..=bottom_right.col)
                .step_by(step_size as usize)
                .all(|col| {
                    let point = Coordinate { col, row };
                    point_in_polygon(&point, polygon)
                })
        })   
}

fn build_polygon(points: Graph<Coordinate, u64>) -> Result<Vec<Coordinate>> {
    let mut coords_left : Vec<Coordinate> = points
        .node_weights()
        .copied()
        .collect();

    let mut polygon : Vec<Coordinate> = vec![coords_left[0]];
    coords_left.remove(0);

    while !coords_left.is_empty() {
        let last_coord = polygon.last().unwrap();
        let next_index = coords_left.iter().position(|c| {
            c.col == last_coord.col || c.row == last_coord.row
        });

        match next_index {
            None => return Err(anyhow!("Couldn't find coordinate")),
            Some(index) => {
                let coord = coords_left.remove(index);
                polygon.push(coord);
            }
        }
    }

    Ok(polygon)
    
}

fn part2(input: &str) -> Result<u64> {
    let coords = parse_coords(input)?;

    let polygon = build_polygon(coords.clone())?;

    println!("Edges: {}", coords.edge_count());
    let mut count = 0;
    let edge = coords.edge_references()
        .filter(|a| {            
            println!("Filtering {} or {}", count, coords.edge_count());
            count += 1;
            let source = coords.node_weight(a.source()).unwrap();
            let target = coords.node_weight(a.target()).unwrap();

            let top_left = Coordinate {
                col: min(source.col, target.col), 
                row: min(source.row,target.row)
            };
            let bottom_right = Coordinate {
                col: max(source.col, target.col), 
                row: max(source.row, target.row)
            };

            rectangle_in_polygon(&top_left, &bottom_right, &polygon)

        })
        .max_by(|a, b| a.weight().cmp(b.weight()))
        .unwrap();

    let area = *edge.weight();
    let source = coords.node_weight(edge.source()).unwrap();
    let target = coords.node_weight(edge.target()).unwrap();

    Ok(area)
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
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 50);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 24);
    }
}