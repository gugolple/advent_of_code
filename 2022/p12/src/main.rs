use std::io;
use std::io::prelude::*;

use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashSet;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Position {
    pos_row: usize,
    pos_col: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Row:{:0>3} Col:{:0>3}", self.pos_row, self.pos_col)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct DijkstraElem {
    cost: u64,
    pos: Position,
}

impl std::fmt::Display for DijkstraElem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "DK Cost:{:0>3} Pos({})", self.cost, self.pos)
    }
}

impl Ord for DijkstraElem {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other.cost.cmp(&self.cost)
            .then_with(|| self.pos.pos_row.cmp(&other.pos.pos_row).then_with(|| self.pos.pos_col.cmp(&other.pos.pos_col)))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for DijkstraElem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn indx(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem) -> char {
    grid[cur_dij.pos.pos_row][cur_dij.pos.pos_col]
}

fn add1_char(c: char) -> char {
    std::char::from_u32(c as u32 + 1).unwrap()
}

fn valid_step(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem, next_dij: &DijkstraElem) -> bool {
    let org = indx(grid, cur_dij);
    let dst = indx(grid, next_dij);

    //println!("Cmp src: {} dst: {}", org, dst);

    // Missing to allow falling!
    match dst {
        'b' ..= 'z' => dst <= add1_char(org), 
        'a' => true,
        'E' => org == 'z',
        'S' => false,
        _ => panic!("Wrong char/not handled!: {}", dst),
    }
}

fn calc_next_movs(grid: &Vec<Vec<char>>, cur_dij: &DijkstraElem) -> Vec<DijkstraElem> {
    let mut result: Vec<DijkstraElem> = Vec::new();
    if cur_dij.pos.pos_row > 0 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos: Position{
                pos_row: cur_dij.pos.pos_row - 1,
                pos_col: cur_dij.pos.pos_col,
            },
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos.pos_row < grid.len()-1 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos: Position{
                pos_row: cur_dij.pos.pos_row + 1,
                pos_col: cur_dij.pos.pos_col,
            },
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos.pos_col > 0 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos: Position{
                pos_row: cur_dij.pos.pos_row,
                pos_col: cur_dij.pos.pos_col - 1,
            },
        };
        if valid_step(grid, cur_dij, &next) {
            result.push(next);
        }
    }
    if cur_dij.pos.pos_col < grid[cur_dij.pos.pos_row].len()-1 {
        let next = DijkstraElem {
            cost: cur_dij.cost + 1,
            pos: Position{
                pos_row: cur_dij.pos.pos_row,
                pos_col: cur_dij.pos.pos_col + 1,
            },
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
        pos: Position {
            pos_row: pos_row,
            pos_col: pos_col,
        },
    }); 

    let mut seen: HashSet<Position> = HashSet::new();
    let mut count_itr = 0;

    while let Some(cur_dij) = heap.pop() {
        if seen.contains(&cur_dij.pos) {
            continue; 
        }
        seen.insert(cur_dij.pos);
        let next_moves = calc_next_movs(&grid, &cur_dij);

        println!("Recur: {}, chr: {}, next: {:0>3}", cur_dij, indx(&grid, &cur_dij), next_moves.len());

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

    for pos in &seen {
        grid[pos.pos_row][pos.pos_col] = '~';
    }

    for row in &grid {
        println!("{}",row.iter().collect::<String>());
    }

    panic!("Path not found! D:");
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
