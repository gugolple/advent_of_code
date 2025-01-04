use std::io;
use std::io::prelude::*;
use std::collections::HashSet;

fn process_input(input: &str) -> i32 {
    let mut result: i32 = 0;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let char_vec: Vec<char> = line.chars().collect();
            println!("CharVec!: {}", char_vec.len());
            let left: HashSet<_> = HashSet::from_iter(char_vec[..char_vec.len()/2].into_iter().collect::<Vec<_>>());
            let right: HashSet<_> = HashSet::from_iter(char_vec[char_vec.len()/2 .. ].into_iter().collect::<Vec<_>>());

            let int = left.intersection(&right).collect::<Vec<_>>();
            // Just safety check that we only have 1 item
            assert_eq!(int.len(),1);

            let int_chr = int[0];
            println!("Int!: {}", int_chr);


            let mut res_val: i32 = 0;
            if int_chr.is_lowercase() {
               res_val = **int_chr as i32 - 'a' as i32 + 1;
            } else{
                res_val = **int_chr as i32 - 'A' as i32 + 27;
            }

            result = result + res_val;
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
        let input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";
        assert_eq!(process_input(input),157);
    }
}
