use std::io;
use std::io::prelude::*;
use std::cmp::Ordering;

fn pair_check(left: &Vec<&str>, right: &Vec<&str>) -> bool {
    //println!("Pair test!");
    //println!("Left: {:?}", left);
    //println!("Right: {:?}", right);
    let mut lit = left.iter();
    let mut ldepth = 0;
    let mut ldepthcheck = false;
    let mut rit = right.iter();
    let mut rdepth = 0;
    let mut rdepthcheck = false;

    loop {
        let lc = lit.next();
        let rc = rit.next();

        print!("L: {:?} -- R: {:?} ____ ", lc, rc);

        if lc.is_none() && !rc.is_none() {
            return true;
        } else if !lc.is_none() && rc.is_none() {
            return false;
        } else if lc.is_none() && rc.is_none() {
            panic!("Empty both!");
        } else {
            let lstr = lc.expect("Already confirmed");
            let rstr = rc.expect("Already confirmed");
            ldepth = ldepth + lstr.matches('[').count();
            rdepth = rdepth + rstr.matches('[').count();

            let ldepthred = lstr.matches(']').count();
            let rdepthred = rstr.matches(']').count();

            let lval = lstr.replace(&['[',']'][..],"").parse::<usize>();
            let rval = rstr.replace(&['[',']'][..],"").parse::<usize>();

            print!("Depth L: {} R: {} ____ Vals L: {:?} R: {:?}", ldepth, rdepth, lval, rval);

            if ldepth == rdepth {
                ldepthcheck = false;
                rdepthcheck = false;
                if lval.is_ok() && rval.is_ok(){
                    match lval.expect("Checked").cmp(&rval.expect("Checked")) {
                        Ordering::Less => return true,
                        Ordering::Greater => return false,
                        Ordering::Equal => (),
                    }
                } else if !lval.is_ok() && rval.is_ok(){
                    return true;
                } else if lval.is_ok() && !rval.is_ok(){
                    return false;
                }
            } else {
                let delta = (ldepth as i64 - rdepth as i64).abs();
                if ldepth < rdepth {
                    if ldepthred == 0 {
                        return false;
                    }
                    if ldepthcheck {
                        return true;
                    }
                    if delta > 1 {
                        return true;
                    }
                    ldepthcheck = true;
                } else {
                    if rdepthred == 0 {
                        return true;
                    }
                    if rdepthcheck {
                        return false;
                    }
                    if delta > 1 {
                        return false;
                    }
                    rdepthcheck = true;
                }
                if lval.is_ok() && rval.is_ok(){
                    match lval.expect("Checked").cmp(&rval.expect("Checked")) {
                        Ordering::Less => return true,
                        Ordering::Greater => return false,
                        Ordering::Equal => (),
                    }
                } else if !lval.is_ok() && rval.is_ok() {
                    return true;
                } else if lval.is_ok() && !rval.is_ok() {
                    return false;
                } else {
                    if ldepth < rdepth {
                        return true;
                    } else {
                        return false;
                    }
                }
            }
            ldepth = ldepth - ldepthred;
            rdepth = rdepth - rdepthred;
        }
        println!("");
    }
}

fn process_input(input: &str) -> u64 {
    let mut result = 0;
    let mut pair_num = 1;
    let mut left: Vec<&str> = Vec::new();
    let mut right: Vec<&str> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            let tmp_desc: Vec<&str> = line.split(',').collect();
            if tmp_desc.len() == 0 {
                panic!("BAD PARSING!");
            }

            if left.len() == 0 {
                left = tmp_desc;
            } else if right.len() == 0 {
                right = tmp_desc;
            } else {
                panic!("Pair not closed!");
            }
        } else {
            // Main logic due to pair wise checking, at least a delimitor
            println!("Pair num: {}", pair_num);
            if pair_check(&left, &right) {
                println!("");
                println!("Pair ordered!");
                result = result + pair_num;
            } else {
                println!("");
                println!("Pair NOT ordered!");
            }
            left.clear();
            right.clear();
            pair_num = pair_num + 1;
        }
    }
    if left.len() > 0 || right.len() > 0 {
        println!("outsideeer");
        if pair_check(&left, &right) {
            println!("");
            println!("Pair ordered!");
            result = result + pair_num;
        } else {
            println!("");
            println!("Pair NOT ordered!");
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
        let input = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";
        assert_eq!(process_input(input),13,"Test has failed");
    }

    #[test]
    fn test_advent_basic1() {
        let input = "[1,1,3,1,1]
[1,1,5,1,1]";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basic2() {
        let input = "[[1],[2,3,4]]
[[1],4]";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basic3() {
        let input = "[9]
[[8,7,6]]";
        assert_eq!(process_input(input),0,"Test has failed");
    }

    #[test]
    fn test_advent_basic4() {
        let input = "[[4,4],4,4]
[[4,4],4,4,4]";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basic5() {
        let input = "[7,7,7,7]
[7,7,7]";
        assert_eq!(process_input(input),0,"Test has failed");
    }

    #[test]
    fn test_advent_basic6() {
        let input = "[]
[3]";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basic7() {
        let input = "[[[]]]
[[]] ";
        assert_eq!(process_input(input),0,"Test has failed");
    }

    #[test]
    fn test_advent_basic8() {
        let input = "[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";
        assert_eq!(process_input(input),0,"Test has failed");
    }

    #[test]
    fn test_advent_basic9() {
        let input = "[[]]
[[[]]] ";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basica() {
        let input = "[1]
[[2,3]] ";
        assert_eq!(process_input(input),1,"Test has failed");
    }

    #[test]
    fn test_advent_basicb() {
        let input = "[[2,3]]
[[[2, 3]],3] ";
        assert_eq!(process_input(input),0,"Test has failed");
    }
}
