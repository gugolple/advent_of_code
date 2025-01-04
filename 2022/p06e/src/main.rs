use std::io;
use std::io::prelude::*;
use std::collections::HashSet;


fn process_input(input: &str) -> usize {
    let marker_size = 14;
    let mut result = 0;
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let char_vec: Vec<char> = line.chars().collect();

            for (idx, window) in char_vec.windows(marker_size).enumerate() {
                println!("Window: {:?}", window);
                // Need this step to set HashSet type
                let hs: HashSet<char> = HashSet::from_iter(window.iter().cloned());
                if hs.len() == marker_size {
                    // First good char
                    return idx+marker_size;
                }
            }
        }
    }
    panic!();
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
        let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
        assert_eq!(process_input(input),19);
    }

    #[test]
    fn test_advent_basic1() {
        let input = "bvwbjplbgvbhsrlpgdmjqwftvncz";
        assert_eq!(process_input(input),23);
    }

    #[test]
    fn test_advent_basic2() {
        let input = "nppdvjthqldpwncqszvftbrmjlhg";
        assert_eq!(process_input(input),23);
    }

    #[test]
    fn test_advent_basic3() {
        let input = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
        assert_eq!(process_input(input),29);
    }

    #[test]
    fn test_advent_basic4() {
        let input = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
        assert_eq!(process_input(input),26);
    }
}
