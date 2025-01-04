use std::io;
use std::io::prelude::*;


fn process_input(input: &str) -> u64 {
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
        }
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
        let input = "";
        assert_eq!(process_input(input),1);
    }
}
