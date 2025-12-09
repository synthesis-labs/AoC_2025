use std::{cmp::{max, min}, collections::{HashMap, HashSet}};

use anyhow::Result;
use nom::{
    character::complete::{u64 as nom_u64, char as nom_char},
    multi::{many1, separated_list1}
};
use petgraph::Graph;
use petgraph::visit::EdgeRef;
use utils;

const DAY: &str = env!("CARGO_PKG_NAME");

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Coordinate {
    x: u64,
    y: u64,
    z: u64,
}

impl Coordinate {
    fn euclidean_distance(&self, b: &Coordinate) -> f64 {
        let dx = (self.x as f64 - b.x as f64).powi(2);
        let dy = (self.y as f64 - b.y as f64).powi(2);
        let dz = (self.z as f64 - b.z as f64).powi(2);
        (dx + dy + dz).sqrt()        
    }
}

fn euclidean_distance(a: &Coordinate, b: &Coordinate) -> f64 {
    let dx = (a.x as f64 - b.x as f64).powi(2);
    let dy = (a.y as f64 - b.y as f64).powi(2);
    let dz = (a.z as f64 - b.z as f64).powi(2);
    (dx + dy + dz).sqrt()
}

fn parse_dag(input: &str) -> Result<Graph<Coordinate, f64>> {

    let mut parser = many1::<_,_,(),_>(
        separated_list1(nom_char(','), nom_u64)
    );

    let coords: Vec<Coordinate> = input
        .trim()
        .lines()
        .filter_map(|l| parser(l).ok())
        .flat_map(|(_, vecs)| vecs)
        .map(|c| Coordinate { x: c[0], y: c[1], z: c[2] })
        .collect();
    
    let mut graph = Graph::new();
    
    let nodes: Vec<_> = coords.into_iter()
        .map(|coord| graph.add_node(coord))
        .collect();
    
    for i in 0..nodes.len() {
        for j in (i + 1)..nodes.len() {
            let dist = euclidean_distance(
                graph.node_weight(nodes[i]).unwrap(),
                graph.node_weight(nodes[j]).unwrap()
            );
            graph.add_edge(nodes[i], nodes[j], dist);
        }
    }
    
    Ok(graph)
}

fn part_connect(graph: &Graph<Coordinate, f64>, num_to_connect: Option<usize>) -> Result<Vec<Vec<Coordinate>>> {
    let mut edges: Vec<_> = graph.edge_references()
        .map(|edge| (edge.source(), edge.target(), *edge.weight()))
        .collect();
    
    edges.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());

    let closest: Vec<_> = match num_to_connect {
        Some(num) => edges
                                .into_iter()
                                .take(num)
                                .collect(),
        None => edges
                                .into_iter()
                                .collect()

    };

    let mut circuits = HashMap::<Coordinate, (usize, usize)>::new();
    let mut circuit_num = 0_usize;
    let mut node_num = 0_usize;
    for (source, target, _) in closest {
        let s = graph.node_weight(source).unwrap();
        let t = graph.node_weight(target).unwrap();

        match (circuits.get(s), circuits.get(t)) {
            // New circuit
            (None, None) => {
                circuits.insert(*s, (circuit_num, node_num));
                node_num += 1;
                circuits.insert(*t, (circuit_num, node_num));
                node_num += 1;
                circuit_num += 1
            }

            // Join to existing circuit
            (Some(a), None) => {
                circuits.insert(*t, (a.0, node_num));
                node_num += 1;
                circuits.get_mut(s).unwrap().1 = node_num;
                node_num += 1;
            }
            (None, Some(b)) => {
                circuits.insert(*s, (b.0, node_num));
                node_num += 1;
                circuits.get_mut(t).unwrap().1 = node_num;
                node_num += 1;

            }

            // Both in circuit already - if they're different circuits, merge them
            (Some(a), Some(b)) => {
                if a.0 != b.0 {
                    let unique_circuits_before: HashSet<usize> = circuits.values()
                        .map(|(circuit_id, _)| *circuit_id)
                        .collect();

                    let circuit_to_merge = min(b.0, a.0);
                    let target_circuit = max(a.0, b.0);
                    
                    circuits.iter_mut()
                        .filter(|(_, (circuit_id, _))| *circuit_id == circuit_to_merge)
                        .for_each(|(_, (circuit_id, _))| {
                            *circuit_id = target_circuit;
                        });          

                }

                circuits.get_mut(s).unwrap().1 = node_num;
                node_num += 1;
                circuits.get_mut(t).unwrap().1 = node_num;
                node_num += 1;

            }
        }
        let unique_circuits_after: HashSet<usize> = circuits.values()
            .map(|(circuit_id, _)| *circuit_id)
            .collect();

        if unique_circuits_after.len() == 1 && circuits.len() == graph.node_count() {
            println!("Breaking cause all circuits merged {:?} -> {:?}", s, t);
            break;
        }

    }    

    let mut grouped_circuits: Vec<Vec<Coordinate>> = circuits
        .iter()
        .fold(HashMap::new(), |mut acc: HashMap<usize, Vec<(Coordinate, usize)>>, (coord, (circuit_id, order_added))| {
            acc.entry(*circuit_id).or_insert_with(Vec::new).push((*coord, *order_added));
            acc
        })
        .into_iter()
        .map(|(_,mut  coords_with_orders)| {
            coords_with_orders.sort_by_key(|(_, node_num)| *node_num);
            coords_with_orders.into_iter().map(|(coord, _)| coord).collect()
        })
        .collect();

    grouped_circuits.sort_by(|a, b| b.len().cmp(&a.len()));
    
    Ok(grouped_circuits)


}

fn part1(input: &str) -> Result<usize> {
    // Your solution for part 1
    let dag = parse_dag(input)?;

    let connected = part_connect(&dag,Some(10))?;    
    Ok(connected[0].len() * connected[1].len() * connected[2].len())
}

fn part2(input: &str) -> Result<u64> {
    let dag = parse_dag(input)?;
    let connected = part_connect(&dag,None)?;
    let big_circuit = connected.first().unwrap();

    Ok(big_circuit[big_circuit.len() - 2].x * big_circuit[big_circuit.len() - 1].x)
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
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"#;

    #[test]
    fn test_part1() {
        assert_eq!(part1(TEST_INPUT.trim()).unwrap(), 40);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(TEST_INPUT.trim()).unwrap(), 25272);
    }
}