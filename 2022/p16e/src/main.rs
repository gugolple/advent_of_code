use std::io;
use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;

fn input_to_map(input: &str) -> (HashMap<String, Vec<String>>, HashMap<String, u64>) {
    let mut tree: HashMap<String, Vec<String>> = HashMap::new();
    let mut vals: HashMap<String, u64> = HashMap::new();
    for line in input.split("\n") {
        if line != "" {
            //println!("Line!: {}", line);
            let val_dest = line.split("; ").collect::<Vec<_>>();
            //println!("VD: {:?}", val_dest);
            let val = val_dest[0].strip_prefix("Valve ").expect("Bad input").split(" has flow rate=").collect::<Vec<_>>();
            println!("V: {:?}", val);
            let des = val_dest[1].split(" to ").skip(1).collect::<Vec<_>>()[0].strip_prefix("valve").expect("Bad input");
            let dest_vec = des.strip_prefix("s").unwrap_or(des).strip_prefix(" ").expect("Bad input").split(", ").map(|x| x.to_string()).collect::<Vec<_>>();
            println!("D: {:?}", dest_vec);

            let val_value = val[1].parse::<u64>().unwrap();
            if val_value > 0 {
                vals.insert(val[0].to_string(), val_value);
            }
            tree.insert(val[0].to_string(), dest_vec);
        }
    }
    return (tree, vals);
}

fn valid_vals(dest: &Vec<String>, open: HashSet<String>) -> Vec<String> {
    dest.iter().filter(|x| !open.contains(x.as_str())).map(|x| x.to_string()).collect()
}

#[derive(Clone, Debug)]
struct RecurseResult {
    val: u64,
    open: HashSet<String>,
}

fn recurse(tree: &HashMap<String, Vec<String>>, vals: &HashMap<String, u64>, seen: &mut HashMap<(String, i64, u64), RecurseResult>, open: &mut HashSet<String>, node: &str, rem: i64, score: u64) -> RecurseResult {
    //println!("Node: {}, D: {} S: {}", node, rem, score);
    // Rem ==> time remaining
    let seen_tup = (node.to_string(), rem, score);
    if seen.contains_key(&seen_tup) {
        return seen[&seen_tup].clone();
    }
    if rem == 0 {
        return RecurseResult{val: score, open: open.clone()};
    } else if rem < 0 {
        return RecurseResult{val: 0, open: HashSet::new()};
    }

    let mut best_score = RecurseResult{val: 0, open: HashSet::new()};

    // Logic for taking
    if vals.contains_key(node) && !open.contains(node) {
        open.insert(node.to_string());
        let my_score = vals[node] * (rem -1) as u64;
        for n in tree[node].iter() {
            let sc = recurse(tree, vals, seen, open, n, rem-2, score + my_score);
            if sc.val > best_score.val {
                best_score = sc;
            }
        }
        open.remove(node);
    }

    // Logic for walking 
    for n in tree[node].iter() {
        let sc = recurse(tree, vals, seen, open, n, rem-1, score);
        if sc.val > best_score.val {
            best_score = sc;
        }
    }

    seen.insert(seen_tup, best_score.clone());
    return best_score;
}

fn process_input(input: &str) -> u64 {
    let (tree, vals) = input_to_map(input);
    println!("Tree: {:?}", tree);
    println!("Vals: {:?}", vals);
    println!("First!");
    let mut res =  recurse(&tree, &vals, &mut HashMap::new(), &mut HashSet::new(), "AA", 26, 0);
    println!("Second!, first: {:?}", res);
    let res =  recurse(&tree, &vals, &mut HashMap::new(), &mut res.open, "AA", 26, res.val);
    println!("Recurse ret: {:?}", res);
    return res.val;
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
        let input = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";
        assert_eq!(process_input(input),1707);
    }
}
