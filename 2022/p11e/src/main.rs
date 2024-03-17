use std::io;
use std::io::prelude::*;
use std::cell::RefCell;

#[derive(Debug)]
struct MonkeyDesc {
    id: usize,
    items: Vec<i128>,
    operation: Vec<String>,
    test_value: i128,
    true_dest: usize,
    false_dest: usize,
    inspect_count: u128,
}

fn parse_monkey(line: &str) -> MonkeyDesc {
    //println!("Money!: {}", line);
    let description: Vec<_> = line.split("\n").map(|x| x.trim()).collect();
    println!("");
    for item in &description {
        println!("Monke desc: {}", item);
    }

    let monkey_id = description[0].split(" ").skip(1).next().expect("I want to make someone cry").trim_end_matches(':').parse::<usize>().unwrap();
    println!("Id: {}", monkey_id);
    let monkey_items: Vec<i128> = description[1].split(": ").skip(1).next().expect("I want to make someone cry").split(", ").map(|x| x.parse::<i128>().unwrap()).collect();
    println!("Items: {:?}", monkey_items);
    let monkey_op: Vec<_> = description[2].split(" = ").skip(1).next().expect("I want to make someone cry").split(" ").map(|x| x.to_string()).collect();
    println!("Operation: {:?}", monkey_op);
    let monkey_test: i128 = description[3].split("by ").skip(1).next().expect("I want to make someone cry").parse::<i128>().unwrap();
    println!("Test: {}", monkey_test);
    let monkey_true: usize = description[4].split("monkey ").skip(1).next().expect("I want to make someone cry").parse::<usize>().unwrap();
    println!("true: {}", monkey_true);
    let monkey_false: usize = description[5].split("monkey ").skip(1).next().expect("I want to make someone cry").parse::<usize>().unwrap();
    println!("false: {}", monkey_false);

    return MonkeyDesc{
        id: monkey_id,
        items: monkey_items,
        operation: monkey_op,
        test_value: monkey_test,
        true_dest: monkey_true,
        false_dest: monkey_false,
        inspect_count: 0,
    };
}

fn process_input(input: &str, rounds: usize) -> u128 {
    let mut total: u128 = 0;
    let mut monkey_vec: Vec<RefCell<MonkeyDesc>> = Vec::new();
    // This loop is only parsing
    for line in input.split("\n\n") {
        // Full Monkey desc
        if line != "" {
            monkey_vec.push(parse_monkey(line).into());
        }
    }

    // Main loop logic
    for _ in 0 .. rounds {
        // Debug print the status
        for monk in &monkey_vec {
            println!("{:?}", monk);
        }

        // Logic
        for ref_monk in &monkey_vec {
            let mut monk = ref_monk.borrow_mut();
            println!("Monk id: {}", monk.id);
            monk.inspect_count = monk.inspect_count + monk.items.len() as u128;
            for item in &monk.items {
                let left_val: i128 = match monk.operation[0].as_str() {
                    "old" => *item,
                    _ => monk.operation[0].parse::<i128>().unwrap()
                };
                let right_val: i128 = match monk.operation[2].as_str() {
                    "old" => *item,
                    _ => monk.operation[2].parse::<i128>().unwrap()
                };
                // After the inspection, before test
                let new_val: i128 = match monk.operation[1].as_str() {
                    "+" => left_val + right_val,
                    "*" => left_val * right_val,
                    _ => panic!("Bad operation!"),
                };
                
                println!("Old val: {} New val: {}", *item, new_val); 
                //println!("Left val: {} Right val: {}", left_val, right_val); 

                if (new_val % monk.test_value) == 0 {
                    monkey_vec[monk.true_dest].borrow_mut().items.push(new_val);
                } else {
                    monkey_vec[monk.false_dest].borrow_mut().items.push(new_val);
                }
            }
            monk.items.clear()
        }
    }

    monkey_vec.sort_by_key(|x| x.borrow().inspect_count);
    // Debug print the status
    for monk in &monkey_vec {
        println!("{:?}", monk);
    }

    total = monkey_vec[monkey_vec.len()-2].borrow().inspect_count * 
        monkey_vec[monkey_vec.len()-1].borrow().inspect_count;

    return total;
}

fn main() -> io::Result<()> {
    // read into a String, so that you don't need to do the conversion.
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    println!("Total: {}", process_input(&buffer,10000));

    Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_advent_basic() {
        let input = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";
        let rounds = vec![1, 20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000];
        let solutions: Vec<u128> = vec![6*4, 103*99, 5204*5192, 10419*10391, 15638*15593, 20858*20797, 26075*26000, 31294*31204, 36508*36400, 41728*41606, 46945*46807, 52166*52013];
        for (test, (rou, sol)) in rounds.iter().zip(solutions.iter()).enumerate() {
            let res = process_input(&input,*rou);
            assert_eq!(res,*sol, "Failed at test: {}, iteration: {}, Target: {} Obtained: {}", test, *rou, *sol, res);
    }
    }
}
