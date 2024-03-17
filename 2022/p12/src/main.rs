use std::io;
use std::io::prelude::*;

use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Position {
    pos_row: usize,
    pos_col: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct DijkstraElem {
    cost: u64,
    pos: Position,
}

impl Ord for DijkstraElem {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other.cost.cmp(&self.cost)
            .then_with(|| self.pos_row.cmp(&other.pos_row).then_with(|| self.pos_col.cmp(&other.pos_col)))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for DijkstraElem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn indx(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem) -> char {
    grid[cur_dij.pos_row][cur_dij.pos_col]
}

fn add1_char(c: char) -> char {
    std::char::from_u32(c as u32 + 1).unwrap()
}

fn valid_step(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem, next_dij: &DijkstraElem) -> bool {
    let org = indx(grid, cur_dij);
    let dst = indx(grid, next_dij);

    //println!("Cmp src: {} dst: {}", org, dst);

    match dst {
        'b' ..= 'z' => dst == org || dst == add1_char(org), 
        'a' => org == 'S' || org == dst,
        'E' => org == 'z',
        'S' => false,
        _ => panic!("Wrong char/not handled!"),
    }
}

fn calc_next_movs(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem) -> Vec<DijkstraElem> {
    let mut result: Vec<DijkstraElem> = Vec::new();
    if cur_dij.pos_row > 0 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos_row: cur_dij.pos_row - 1,
            pos_col: cur_dij.pos_col,
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos_row < grid.len()-1 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos_row: cur_dij.pos_row + 1,
            pos_col: cur_dij.pos_col,
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos_col > 0 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos_row: cur_dij.pos_row,
            pos_col: cur_dij.pos_col - 1,
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos_col < grid[cur_dij.pos_row].len()-1 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos_row: cur_dij.pos_row,
            pos_col: cur_dij.pos_col + 1,
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    return result;
}

fn process_input(input: &str) -> u64 {
    let mut grid: Vec<Vec<char>> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            grid.push(line.chars().collect());
        }
    }

    let mut pos_row = usize::MAX;
    let mut pos_col = usize::MAX;

    for (row_num, row) in grid.iter().enumerate() {
        if let Some(col_num) = row.iter().position(|x| *x == 'S') {
            pos_col = col_num;
            pos_row = row_num;
        }
    }

    let mut heap = BinaryHeap::new();
    heap.push(DijkstraElem {
        cost: 0,
        pos_row: pos_row,
        pos_col: pos_col,
    }); 

    let mut count_itr = 0;

    while let Some(cur_dij) = heap.pop() {
        let next_moves = calc_next_movs(&grid, &cur_dij);
        println!("Recur: {:?}, next: {}", cur_dij, next_moves.len());

        count_itr = count_itr + 1;
        //if count_itr > 50 {
        //    panic!("Rly!?");
        //}

        for mov in next_moves {
            //println!("    Next!: {:?}", mov);
            if indx(&grid, &mov) == 'E' {
                return mov.cost;
            }
            heap.push(mov);
        };
    }

    return 0;
}

fn main() -> io::Result<()> {
    // read into a String, so that you don't need to do the conversion.
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    println!("Total: {}", process_input(&buffer));

    Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_advent_basic() {
        let input = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";
        assert_eq!(process_input(input),31);
    }
}
