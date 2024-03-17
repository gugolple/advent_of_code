use std::io;
use std::str::FromStr;
use std::io::prelude::*;

fn process_input(input: &str) -> i32 {
    let mut vec: Vec<u32> = Vec::new();
    let mut cur_elf: u32 = 0;
    let lines  = input.split("\n");
    for line in lines {
        if line != "" {
            println!("line: {}", line);
            cur_elf = cur_elf + u32::from_str(line).unwrap();
        } else {
            vec.push(cur_elf);
            cur_elf = 0;
        }
    }
    let mut cur_max: u32 = 0;
    for val in vec.iter() {
        if val > &cur_max {
            cur_max = *val;
        }
    }
    return cur_max.try_into().unwrap();
}

fn main() -> io::Result<()> {
    // read into a String, so that you don't need to do the conversion.
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    println!("{}", &buffer);

    println!("Total: {}", process_input(&buffer));

    Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_advent_basic() {
        let input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";
        assert_eq!(process_input(input),24000);
    }
}
