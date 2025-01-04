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

#[derive(PartialEq)]
enum Action {
    Activate(String),
    Move(String),
}

fn recurse_options(tree: &HashMap<String, Vec<String>>, vals: &HashMap<String, u64>, open: &mut HashSet<String>, node: &str) -> Vec<Action> {
    let mut actions: Vec<Action> = Vec::new();
    // Just recurse
    if vals.contains_key(node) && !open.contains(node) {
        actions.push(Action::Activate(node.to_string()));
    }
    // Move
    for n in tree[node].iter() {
        actions.push(Action::Move(n.clone()));
    }
    return actions;
}


fn recurse(tree: &HashMap<String, Vec<String>>, vals: &HashMap<String, u64>, seen: &mut HashMap<(String, String, i64, u64), u64>, open: &mut HashSet<String>, node: (&str, &str), rem: i64, score: u64) -> u64 {
    //println!("Node: {}, D: {} S: {}", node, rem, score);
    // Rem ==> time remaining
    let seen_tup = (node.0.to_string(), node.1.to_string(), rem, score);
    if seen.contains_key(&seen_tup) {
        return seen[&seen_tup];
    }
    if rem == 0 {
        return score;
    } else if rem < 0 {
        return 0;
    }

    let mut best_score = 0;
    let mut myscore = 0;

    for act1 in recurse_options(tree, vals, open, node.0 ) {
        if let Action::Activate(ref n) = act1 {
            open.insert(n.to_string());
            myscore = myscore + vals[n] * (rem -1) as u64;
        }
        for act2 in recurse_options(tree, vals, open, node.1) {
            if let Action::Activate(ref n) = act2 {
                open.insert(n.to_string());
                myscore = myscore + vals[n] * (rem -1) as u64;
            }
            let next_nodes = [&act1, &act2].iter().map(|x| match x {  Action::Activate(n) => n, Action::Move(n) => n}).map(|x| x.clone()).collect::<Vec<String>>();
            let sc = recurse(tree, vals, seen, open, (next_nodes[0].as_str(), next_nodes[1].as_str()), rem -1, score + myscore);
            if sc > best_score {
                best_score = sc;
            }
            if let Action::Activate(ref n) = act2 {
                open.remove(n.as_str());
                myscore = myscore - vals[n] * (rem -1) as u64;
            }
        }
        if let Action::Activate(ref n) = act1 {
            open.remove(n.as_str());
            myscore = myscore - vals[n] * (rem -1) as u64;
        }
    }
    //println!("L: {} R: {} S: {}", node.0, node.1, best_score);

    seen.insert(seen_tup, best_score);
    return best_score;
}

fn process_input(input: &str) -> u64 {
    let (tree, vals) = input_to_map(input);
    println!("Tree: {:?}", tree);
    println!("Vals: {:?}", vals);
    let rec =  recurse(&tree, &vals, &mut HashMap::new(), &mut HashSet::new(), ("AA", "AA"), 26, 0);
    println!("Recurse ret: {}", rec);
    return rec;
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
