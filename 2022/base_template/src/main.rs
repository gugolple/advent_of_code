use std::io;
use std::io::prelude::*;

fn input_to_coll(input: &str) -> Vec<String> {
    let mut res: Vec<String> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            res.push(line.to_string());
        }
    }
    return res;
}

fn process_input(input: &str) -> u64 {
    let coll = input_to_coll(input);

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
    fn basic() {
        let input = "";
        assert_eq!(process_input(input),1);
    }
}
