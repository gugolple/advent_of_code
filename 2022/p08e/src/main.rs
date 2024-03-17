use std::io;
use std::io::prelude::*;
use std::iter;

struct Position {
    row: usize,
    col: usize,
}

fn calculate_visibility_tree(input: &Vec<Vec<i8>>, pos: &Position) -> u64 {
    let mytreeheight: i8 = input[pos.row][pos.col];
    // Horizontal
    let mut h: u64 = 0;
    if pos.col < input[pos.row].len() {
        for idx in pos.col+1 .. input[pos.row].len() {
            h = h + 1;
            if input[pos.row][idx] >= mytreeheight {
                break;
            }
        }
    }
    // Horizontal reverse
    let mut hr: u64 = 0;
    if pos.col > 0 {
        for idx in (0 ..= pos.col-1).rev() {
            hr = hr + 1;
            if input[pos.row][idx] >= mytreeheight {
                break;
            }
        }
    }
    // Vertical
    let mut v: u64 = 0;
    if pos.row < input.len() {
        for idx in pos.row+1 .. input.len() {
            v = v + 1;
            if input[idx][pos.col] >= mytreeheight {
                break;
            }
        }
    }
    // Vertical reverse
    let mut vr: u64 = 0;
    if pos.row > 0 {
        for idx in  (0 ..= pos.row-1).rev() {
            vr = vr + 1;
            if input[idx][pos.col] >= mytreeheight {
                break;
            }
        }
    }
    println!("Pos row: {} col: {} ::: Results: Up: {}, Left: {}, Right: {}, Down: {}", pos.row, pos.col, vr, hr, h, v);
    let total = [h, hr, v, vr].into_iter().reduce(|a,b| a * b);
    return total.unwrap();
}

fn calculate_visibility(input: &Vec<Vec<i8>>) -> u64 {
    let mut best_tree: u64 = 0;
    for row in 0 .. input.len() {
        for col in 0 .. input[row].len() {
            let tree_vis = calculate_visibility_tree(input, &Position{row: row, col: col});
            if tree_vis > best_tree {
                best_tree = tree_vis;
            }
        }
    }
    return best_tree;
}

fn clone_row(input: &Vec<i8>) -> Vec<i8> {
    iter::repeat(0).take(input.len()).collect()
}

fn clone_grid(input: &Vec<Vec<i8>>) -> Vec<Vec<i8>> {
    iter::repeat(clone_row(&input[0])).take(input.len()).collect()
}

fn process_input(input: &str) -> u64 {
    let mut grid: Vec<Vec<i8>> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let char_vec: Vec<char> = line.chars().collect();

            let row: Vec<i8> = char_vec.iter().map(|c| c.to_string().parse::<i8>().unwrap()).collect();
            grid.push(row);
        }
    }

    println!("");
    println!("Grid:");
    for row in &grid {
        println!("Row: {:?}", row);
    }

    let mut total = 0;
    total = total + calculate_visibility(&grid);
    return total;
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
        let input = "30373
25512
65332
33549
35390";
        assert_eq!(process_input(input),8);
    }
}
