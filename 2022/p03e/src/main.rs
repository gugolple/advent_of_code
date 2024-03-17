use std::io;
use std::io::prelude::*;
use std::collections::HashSet;

fn process_input(input: &str) -> i32 {
    let mut result: i32 = 0;
    let mut elf_vec_set: Vec<HashSet<char>> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            let char_vec: Vec<char> = line.chars().collect();
            let elf_hash: HashSet<_> = HashSet::from_iter(char_vec.into_iter().collect::<Vec<char>>());
            elf_vec_set.push(elf_hash);
        }
    }

    for (idx1, elf1) in elf_vec_set.iter().step_by(3).enumerate() {
        let elf2 = &elf_vec_set[idx1*3+1];
        let elf_int = elf1.intersection(&elf2).cloned().collect::<Vec<char>>();
        let elf_int_set: HashSet<char> = HashSet::from_iter(elf_int);
        let elf3 = &elf_vec_set[idx1*3+2];
        let elf_tint = elf3.intersection(&elf_int_set).cloned().collect::<Vec<char>>();
        assert_eq!(elf_tint.len(), 1);
        let int_chr = elf_tint[0];
        println!("Single match: {}", int_chr);
        let mut res_val: i32 = 0;
        if int_chr.is_lowercase() {
            res_val = int_chr as i32 - 'a' as i32 + 1;
        } else{
            res_val = int_chr as i32 - 'A' as i32 + 27;
        }
        result = result + res_val;
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
        assert_eq!(process_input(input),70);
    }
}
