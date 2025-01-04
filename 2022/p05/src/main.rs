use std::io;
use std::io::prelude::*;

enum input_state {
    read_stacks, read_operations
}

fn process_input(input: &str) -> String {
    let mut mis: input_state = input_state::read_stacks;
    let mut stacks: Vec<Vec<char>> = Vec::new();
    let mut identifiers: Vec<char> = Vec::new();
    let mut prepared = false;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            match mis {
                input_state::read_stacks => {
                    println!("Stack");
                    let char_vec: Vec<char> = line.chars().skip(1).step_by(4).collect();
                    for (idx, chr) in char_vec.into_iter().enumerate() {
                        if stacks.len() <= idx {
                            stacks.push(Vec::new());
                        }
                        if chr != ' ' {
                            stacks[idx].push(chr);
                        }
                        println!("Size: {}", stacks[idx].len());
                    }
                    println!("");

                }
                input_state::read_operations => {
                    println!("Operation");
                    let operations: Vec<&str> = line.split(" ").collect();
                    let amount = operations[1].parse::<usize>().unwrap();
                    let source = operations[3].parse::<usize>().unwrap() -1;
                    let destin = operations[5].parse::<usize>().unwrap() -1;
                    println!("S: {} D: {} A: {}", source, destin, amount);
                    if source != destin {
                        let mut tmp_v: Vec<char> = Vec::new();
                        let old_size = &stacks[source].len();
                        for i in 0..amount {
                            tmp_v.push(stacks[source].remove(old_size-1-i));
                        }
                        for chr in tmp_v {
                            stacks[destin].push(chr);
                        }
                    }
                }
            };
        } else {
            if !prepared {
                mis = input_state::read_operations;
                println!("Preparing stacks!");
                for stack in &mut stacks {
                    identifiers.push(stack.pop().expect("Cannot fail?"));
                    stack.reverse();
                    for chr in stack {
                        print!("{} ", chr);
                    }
                    println!();
                }
                prepared = true;
            }
        }
    }


    let mut str_res: String = String::new();
    for stack in stacks {
        println!("Stack!");
        for chr in &stack {
            print!("{} ", chr);
        }
        println!();
        str_res = str_res + &stack.last().unwrap_or(&' ').to_string();
    }

    return str_res;
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
        let input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";
        assert_eq!(process_input(input),"CMZ");
    }
}
