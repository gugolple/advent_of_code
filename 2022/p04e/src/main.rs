use std::io;
use std::io::prelude::*;

struct Assigment {
    beg: u32,
    end: u32,
}

fn string_to_assigment(raw_assigment: &str) -> Assigment {
    let begin_end: Vec<_> = raw_assigment.split("-").collect();
    Assigment {
        beg: begin_end[0].parse::<u32>().unwrap(),
        end: begin_end[1].parse::<u32>().unwrap(),
    }
}

fn process_input(input: &str) -> i32 {
    let mut result: i32 = 0;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let assigments:Vec<_> = line.split(",").collect();
            let assigment:Vec<Assigment> = assigments.into_iter().map(|a| string_to_assigment(a)).collect(); 
            let first = &assigment[0];
            let second = &assigment[1];

            if first.beg == second.beg && first.end == second.end {
                result = result + 1;
            } else if first.beg <= second.beg && first.end >= second.end {
                result = result + 1;
            } else if first.beg >= second.beg && first.end <= second.end {
                result = result + 1;
            } else if first.beg <= second.beg && first.end >= second.beg {
                result = result + 1;
            } else if first.beg <= second.end && first.end >= second.end {
                result = result + 1;
            }
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
        let input = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";
        assert_eq!(process_input(input),4);
    }

    #[test]
    fn test_advent_extra() {
        let input = "4-5,2-8";
        assert_eq!(process_input(input),1);
    }

    #[test]
    fn test_advent_extra1() {
        let input = "4-4,4-4";
        assert_eq!(process_input(input),1);
    }
}
