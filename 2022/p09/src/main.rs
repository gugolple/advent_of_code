use std::io;
use std::io::prelude::*;
use std::collections::HashSet;

#[derive(Eq, Hash, PartialEq, Clone)]
struct Position {
    hor: i64,
    ver: i64,
}

fn add(left: &Position, right: &Position) -> Position {
    Position {
        hor: left.hor + right.hor,
        ver: left.ver + right.ver,
    }
}

fn position_tail(head: &Position, tail: &Position) -> Position {
    let delta_h = head.hor - tail.hor;
    let delta_v = head.ver - tail.ver;

    if delta_h > 2 || delta_v > 2 || (delta_h == 2 && delta_v == 2) {
        panic!("Too far!");
    }

    let change = delta_h.abs() > 1 || delta_v.abs() > 1;

    let mut next_hor = 0;
    let mut next_ver = 0;

    if change {
        //println!("Change trigger!");
        next_hor = match delta_h {
            1 | 2 => 1,
            -1 | -2 => -1,
            0 => 0, 
            _ => panic!("Horizontal dichaster!"),
        };
        next_ver = match delta_v {
            1 | 2 => 1,
            -1 | -2 => -1,
            0 => 0, 
            _ => panic!("Vertical dichaster!"),
        };
    }

    Position {
        hor: next_hor + tail.hor,
        ver: next_ver + tail.ver,
    }
}

fn process_input(input: &str) -> u64 {
    let mut head_pos = Position{ hor: 0, ver: 0};
    let mut tail_pos = Position{ hor: 0, ver: 0};
    let mut seen: HashSet<Position> = HashSet::new();
    seen.insert(Position{hor: 0, ver: 0});
    for line in input.split("\n") {
        if line != "" {
            //println!("Line!: {}", line);
            let operation_factors: Vec<_> = line.split(" ").collect();
            let dir = operation_factors[0];
            let steps = operation_factors[1].parse::<u64>().unwrap();
            let direction: &Position = match dir {
                "U" => &Position{hor: 0, ver: -1},
                "D" => &Position{hor: 0, ver: 1},
                "L" => &Position{hor: -1, ver: 0},
                "R" => &Position{hor: 1, ver: 0},
                _ => panic!("Bad direction!")
            };
            for _ in 0 .. steps {
                head_pos = add(&head_pos, direction);
                //println!("  step head hor: {} ver: {}", head_pos.hor, head_pos.ver);
                tail_pos = position_tail(&head_pos, &tail_pos);
                //println!("  step tail hor: {} ver: {}", tail_pos.hor, tail_pos.ver);
                seen.insert(tail_pos.clone());
            }
            //println!("Cur head hor: {} ver: {}", head_pos.hor, head_pos.ver);
            //println!("Cur tail hor: {} ver: {}", tail_pos.hor, tail_pos.ver);
        }
    }
    return seen.len() as u64;
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
        let input = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";
        assert_eq!(process_input(input),13);
    }
}
