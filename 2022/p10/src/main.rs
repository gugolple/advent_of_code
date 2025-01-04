use std::io;
use std::io::prelude::*;

fn process_input(input: &str) -> i64 {
    let check_cycles: Vec<i64> = vec![20,60,100,140,180,220];
    let mut total: i64 = 0;
    let mut cycle: i64 = 0;
    let mut reg: i64 = 1;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {} Reg: {}", line, reg);
            let operation: Vec<_> = line.split(" ").collect();
            let dly = match operation[0] {
                "noop" => 1,
                "addx" => 2,
                _ => panic!("Wrong instruction!"),
            };
            for _ in 0 .. dly {
                cycle = cycle + 1;
                if check_cycles.contains(&cycle) {
                    println!("---- Cycle: {} reg: {}", cycle, reg);
                    total = total + cycle * reg;
                }
            }
            match operation[0] {
                "noop" => (),
                "addx" => reg = reg + operation[1].parse::<i64>().unwrap(),
                _ => panic!("Wrong instruction!"),
            };
        }
    }
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
        let input = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";
        assert_eq!(process_input(input),13140);
    }
}
