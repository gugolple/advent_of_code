use std::io;
use std::io::prelude::*;
use std::iter;
use std::cmp;


#[derive(Debug, Copy, Clone)]
struct Position {
    row: usize,
    col: usize,
}

fn add(left: &Position, right: &Position) -> Position{
    Position{
        row: left.row + right.row,
        col: left.col + right.col,
    }
}

fn print_grid(grid: &Vec<Vec<char>>) {
    println!("Grid:");
    for ci in 0 .. grid[0].len() {
        println!("Row {:0>3}: {:?}", ci, grid.iter().map(|x| x[ci]).collect::<String>());
    }
}

fn transpose_grid(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut result: Vec<Vec<char>> = Vec::new();
    for ci in 0 .. grid[0].len() {
        result.push(grid.iter().map(|x| x[ci]).collect::<Vec<char>>());
    }
    return result;
}

fn grid_from_size(size: &Position) -> Vec<Vec<char>> {
    iter::repeat(
        iter::repeat('.').take(size.col).collect()
        ).take(size.row).collect()    
}

fn set_grid_from_rock_lines(grid: &mut Vec<Vec<char>>, rock_lines: &Vec<Vec<Position>>, start_coor: &Position) {
    for rock_line in rock_lines {
        for window in rock_line.windows(2) {
            let start = &window[0];
            let end = &window[1];

            //println!("SR: {:?} DR: {:?}", start, end);

            let start_pos = Position{
                row: cmp::min(start.row, end.row) - start_coor.row,
                col: cmp::min(start.col, end.col) - start_coor.col,
            };

            let end_pos = Position{
                row: cmp::max(start.row, end.row) - start_coor.row,
                col: cmp::max(start.col, end.col) - start_coor.col,
            };

            println!("Start: {:?} Dest: {:?}", start_pos, end_pos);

            for row_idx in start_pos.row ..= end_pos.row {
                grid[row_idx][start_pos.col] = '#';
            }
            for col_idx in start_pos.col ..= end_pos.col {
                grid[start_pos.row][col_idx] = '#';
            }

            print_grid(grid);
        }
    }
}

fn get_grid(grid: &mut Vec<Vec<char>>, rel: &Position) -> char {
    return grid[rel.row][rel.col];
}

fn sand_simulation(grid: &mut Vec<Vec<char>>, sand_start_rel: &Position) -> u64 {
    let mut count: u64 = 0;
    let drop = Position{
        row: 0,
        col: 1,
    };
    'big: loop {
        count = count + 1;

        let mut pos: Position = *sand_start_rel;
        'drop_sand: loop {
            let mut next_pos = add(&pos, &drop);
            println!("NP: {:?}", next_pos);
            if next_pos.col == grid[next_pos.row].len() {
                break 'big;
            }
            match get_grid(grid, &next_pos) {
                '.' => pos = next_pos,
                '#' | 'o' => {
                    // Left
                    let mut left_valid = true;
                    if next_pos.row > 0 {
                        let left_pos = Position{row: next_pos.row-1, col: next_pos.col};
                        match get_grid(grid, &left_pos) {
                            '.' => pos = left_pos,
                            '#' | 'o' => left_valid = false,
                            _ => panic!("Bad grid!"),
                        };
                    } else {
                        break 'big;
                    }
                    let mut right_valid = true;
                    // Right
                    if next_pos.row < grid.len()-1 {
                        if ! left_valid {
                            let right_pos = Position{row: next_pos.row+1, col: next_pos.col};
                            match get_grid(grid, &right_pos) {
                                '.' => pos = right_pos,
                                '#' | 'o' => right_valid = false,
                                _ => panic!("Bad grid!"),
                            };
                        };
                    } else {
                        break 'big;
                    }
                    if !left_valid && !right_valid {
                        grid[pos.row][pos.col] = 'o';
                        break 'drop_sand;
                    }
                },
                _ => panic!("Bad grid!"),
            };
        }
        print_grid(grid);
    }
    // We exit when the first sand fell outside
    return count -1;
}

fn process_input(input: &str) -> u64 {
    let sand_pos = Position { row: 500, col: 0};
    let mut rock_lines: Vec<Vec<Position>> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            //println!("Line!: {}", line);
            let mut rl: Vec<Position> = Vec::new();
            for pos in line.split(" -> ") {
                //println!("Split!: {}", pos);
                let mut num = pos.split(",");
                rl.push(Position{
                    row: num.next().expect("Should exist").parse::<usize>().unwrap(),
                    col: num.next().expect("Should exist").parse::<usize>().unwrap(),
                });
            }
            rock_lines.push(rl);
        }
    }

    // Set starting to sand start position, because that must be part

    let mut start_coordinates = Position{ row: sand_pos.row, col: sand_pos.col};
    let mut end_coordinates = Position{ row: sand_pos.row, col: sand_pos.col};

    for line in &rock_lines {
        println!("L: {:?}", line);
        for pos in line {
            start_coordinates.row = if start_coordinates.row > pos.row {pos.row} else {start_coordinates.row};
            start_coordinates.col = if start_coordinates.col > pos.col {pos.col} else {start_coordinates.col};
            end_coordinates.row = if end_coordinates.row < pos.row {pos.row} else {end_coordinates.row};
            end_coordinates.col = if end_coordinates.col < pos.col {pos.col} else {end_coordinates.col};
        }
    }

    let size = Position {
        row: end_coordinates.row - start_coordinates.row + 1,
        col: end_coordinates.col - start_coordinates.col + 1,
    };

    println!("Start: {:?}", start_coordinates);
    println!("End: {:?}", end_coordinates);
    println!("Size: {:?}", size);

    let mut grid = grid_from_size(&size);
    grid[500-start_coordinates.row][0] = '+';
    set_grid_from_rock_lines(&mut grid, &rock_lines, &start_coordinates);
    print_grid(&grid);

    let sand_rel = Position {
        row: sand_pos.row - start_coordinates.row,
        col: sand_pos.col - start_coordinates.col,
    };
    return sand_simulation(&mut grid, &sand_rel);
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
        let input = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";
        assert_eq!(process_input(input),93);
    }
}
