use std::io;
use std::io::prelude::*;
use std::iter;

fn calculate_visibility_horizontal(input: &Vec<Vec<i8>>, seen: &mut Vec<Vec<i8>>) -> u64 {
    let mut tot_vis: u64 = 0;
    for (row_num, row) in input.iter().enumerate() {
        let mut cur_tree = -1;
        for (idx, tree) in row.iter().enumerate() {
            if *tree > cur_tree {
                cur_tree = *tree;
                if seen[row_num][idx] == 0 {
                    tot_vis = tot_vis + 1;
                    seen[row_num][idx] = 1;
                }
            }
        }
        let mut cur_tree = -1;
        for (idx, tree) in row.iter().enumerate().rev() {
            if *tree > cur_tree {
                cur_tree = *tree;
                if seen[row_num][idx] == 0 {
                    tot_vis = tot_vis + 1;
                    seen[row_num][idx] = 1;
                }
            }
        }
    }
    return tot_vis;
}

fn calculate_visibility_vertical(input: &Vec<Vec<i8>>, seen: &mut Vec<Vec<i8>>) -> u64 {
    let mut tot_vis: u64 = 0;
    for (row_num, _row) in input[0].iter().enumerate() {
        let mut cur_tree: i8 = -1;
        for idx in 0 .. input.len() {
            let grid_tree: i8 = (*input)[idx][row_num];
            if grid_tree > cur_tree {
                cur_tree = grid_tree;
                if seen[idx][row_num] == 0 as i8{
                    tot_vis = tot_vis + 1;
                    seen[idx][row_num] = 1 as i8;
                }
            }
        }
        let mut cur_tree: i8 = -1;
        for idx in (0 .. input.len()).rev() {
            let grid_tree: i8 = (*input)[idx][row_num];
            if grid_tree > cur_tree {
                cur_tree = grid_tree;
                if seen[idx][row_num] == 0 as i8{
                    tot_vis = tot_vis + 1;
                    seen[idx][row_num] = 1 as i8;
                }
            }
        }
    }
    return tot_vis;
}

fn calculate_visibility(input: &Vec<Vec<i8>>) -> u64 {
    let mut seen_grid = clone_grid(input);
    let mut tot = calculate_visibility_horizontal(input, &mut seen_grid);
    tot = tot + calculate_visibility_vertical(input, &mut seen_grid);

    for row in &seen_grid {
        println!("Seen row: {:?}", row);
    }
    return tot;
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
        assert_eq!(process_input(input),21);
    }

    #[test]
    fn test_advent_basic1() {
        let input = "90009
90009
90109
90009
90009";
        assert_eq!(process_input(input),17);
    }
}
