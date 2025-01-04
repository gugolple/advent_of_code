use std::io;
use std::io::prelude::*;
use std::cmp::Ordering;

#[derive(PartialEq, Debug, Clone)]
enum ListItemDef {
    Num(i32),
    SubList(Vec<ListItemDef>),
    Empty,
}

fn recursive_parse_aux(itr: &Vec<char>, idx: usize) -> (ListItemDef, usize) {
    let first = itr[idx];
    //println!("First: {:?}", first);
    let mut result;
    let mut counter = 1;
    if first == '[' {
        result = ListItemDef::SubList(Vec::new());
        //println!("Recursion!");
        while idx+counter < itr.len() {
            let nc = itr[idx + counter];
            let (nlid, ncounter) = match nc {
                '[' => recursive_parse_aux(itr, idx+counter),
                ']' => {
                    counter = counter + 1;
                    break;
                },
                ',' | ' ' => (ListItemDef::Empty, 1),
                '0' ..= '9' => recursive_parse_aux(itr, idx+counter),
                _ => panic!("What did come?: {}", nc),
            };
            match nlid {
                ListItemDef::Empty => (),
                _ => {
                    if let ListItemDef::SubList(ref mut tv) = result {
                        tv.push(nlid);
                    }
                },
            };
            counter = counter + ncounter;
        }
    } else {
        let mut mstr: Vec<char> = Vec::new();
        mstr.push(first);
        while  idx+counter < itr.len() {
            let nc = itr[idx + counter];
            if nc.is_digit(10) {
                mstr.push(nc);
            } else {
                break;
            }
            counter = counter + 1;
        }
        result = ListItemDef::Num(mstr.iter().collect::<String>().parse::<i32>().expect("Bad parsing"));
    }
    return (result, counter);
}

fn recursive_parse(itr: &Vec<char>) -> ListItemDef {
    return recursive_parse_aux(itr, 0).0;
}


fn pair_check(left: &ListItemDef, right: &ListItemDef) -> Ordering {
    //println!("Cmp");
    //println!("  L: {:?}", left);
    //println!("  R: {:?}", right);
    match left {
        ListItemDef::SubList(l) => {
            match right {
                ListItemDef::SubList(r) => {
                    // Both are lists!
                    for (litm, ritm) in l.iter().zip(r.iter()) {
                        let pcr = pair_check(litm, ritm);
                        if pcr != Ordering::Equal {
                            return pcr;
                        }
                    }
                    l.len().cmp(&r.len())
                },
                ListItemDef::Num(r) => pair_check(left, &ListItemDef::SubList(vec![ListItemDef::Num(*r)])),
                ListItemDef::Empty => panic!("Bad data"),
            }
        },
        ListItemDef::Num(l) => {
            match right {
                ListItemDef::SubList(_r) => pair_check(&ListItemDef::SubList(vec![ListItemDef::Num(*l)]), right),
                ListItemDef::Num(r) => l.cmp(r),
                ListItemDef::Empty => panic!("Bad data"),
            }
        },
        ListItemDef::Empty => panic!("Bad data"),
    }
}


fn process_input(input: &str) -> u64 {
    let mut result = 1;
    let mut packets: Vec<ListItemDef> = Vec::new();
    let mut my_packets: Vec<_> = Vec::new();
    my_packets.push(recursive_parse(&"[[2]]".chars().collect::<Vec<char>>()));
    my_packets.push(recursive_parse(&"[[6]]".chars().collect::<Vec<char>>()));
    for packet in &my_packets {
        packets.push(packet.clone());
    }
    for line in input.split("\n") {
        if line != "" {
            println!("Line: {}", line);

            let tmp_desc = recursive_parse(&line.chars().collect());

            println!("Parsed: {:?}", tmp_desc);

            packets.push(tmp_desc);
        }
    }

    packets.sort_by(|a, b| pair_check(a, b));

    for packet in &my_packets {
        result = result * (packets.iter().position(|p| p == packet).unwrap()+1) as u64;
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
        assert_eq!(process_input(input),140,"Test has failed");
    }
}
