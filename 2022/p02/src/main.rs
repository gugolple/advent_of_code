use std::io;
use std::io::prelude::*;

enum Possibilities {
    Rock, Paper, Scissors
}

fn charToPosibilities(c: &str) -> Possibilities {
    match c {
        "A" => Possibilities::Rock,
        "B" => Possibilities::Paper,
        "C" => Possibilities::Scissors,
        "X" => Possibilities::Rock,
        "Y" => Possibilities::Paper,
        "Z" => Possibilities::Scissors,
        _ => panic!()
    }
}

fn combat(l: Possibilities, r: Possibilities) -> u8 {
    match l {
        Possibilities::Rock => match r {
            Possibilities::Rock => 1 + 3,
            Possibilities::Paper => 2 + 6,
            Possibilities::Scissors => 3
        },
        Possibilities::Paper => match r {
            Possibilities::Rock => 1 ,
            Possibilities::Paper => 2 + 3,
            Possibilities::Scissors => 3 + 6
        },
        Possibilities::Scissors => match r {
            Possibilities::Rock => 1 + 6,
            Possibilities::Paper => 2,
            Possibilities::Scissors => 3 + 3
        }
    }
}

fn process_input(input: &str) -> i32 {
    let mut result: i32 = 0;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let inputs: Vec<_> = line.split(" ").collect();
            let l = charToPosibilities(inputs[0]);
            let r = charToPosibilities(inputs[1]);

            result = result + combat(l,r) as i32;
        }
    }
    return result;
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
        let input = "A Y
B X
C Z";
        assert_eq!(process_input(input),2+6 + 1 + 3+3);
    }

    #[test]
    fn test_advent_basic_1() {
        let input = "A X
A Y
A Z";
        assert_eq!(process_input(input),1+3 + 2+6 + 3);
    }

    #[test]
    fn test_advent_basic_2() {
        let input = "B X
B Y
B Z";
        assert_eq!(process_input(input),1 + 2+3 + 3+6);
    }

    #[test]
    fn test_advent_basic_3() {
        let input = "C X
C Y
C Z";
        assert_eq!(process_input(input),1+6 + 2 + 3+3);
    }
}
