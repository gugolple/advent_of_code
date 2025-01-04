use std::io;
use std::io::prelude::*;
use std::rc::Rc;
use std::cell::RefCell;

struct FsDesc {
    parent: Option<Rc<FsDesc>>,
    name: String,
    size: RefCell<u64>,
    children: RefCell<Vec<Rc<FsDesc>>>,
}

fn recurse_set_sizes(root: Rc<FsDesc>, thresh: u64) -> u64{
    let mut recurse_size = 0;
    let mut tot_size: u64 = 0;
    println!("Dir name: {}", root.name);
    for child in &*root.children.borrow() {
        if child.children.borrow().len() == 0 {
            println!("File {} {}", child.name, child.size.borrow());
            tot_size = tot_size + *child.size.borrow();
        } else {
            let temp_recurse_size = recurse_set_sizes(child.clone(), thresh);
            // Add the recursion to OUR OWN size
            tot_size = tot_size + temp_recurse_size;
            // Keep the recursion size
            recurse_size = recurse_size + temp_recurse_size;
            println!("Back at dir {}", root.name);
        }
    }
    println!("Res dir name: {} size: {}", root.name, tot_size);
    if tot_size < thresh {
        recurse_size = recurse_size + tot_size;
    }
    *root.size.borrow_mut() = tot_size;
    return recurse_size;
}

fn process_input(input: &str) -> u64 {
    let root: Rc<FsDesc> =  Rc::new(FsDesc{
        parent: None,
        name: "root".to_string(),
        size: 0.into(),
        children: Vec::new().into(),
    });
    let mut current: Rc<FsDesc> = Rc::clone(&root);
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let parts: Vec<_> = line.split(" ").collect();
            if parts[0] == "$" {
                match parts[1] {
                    "cd" => {
                        // If we are not going up the tree
                        if parts[2] != ".." && parts[2] != "/" {
                            let next_current = current.children.borrow().iter().find(|x| x.name == parts[2]).expect("It should have been given by the LS").clone();
                            current = next_current;
                        } else {
                            // Not handling the ROOT, special case
                            if parts[2] != "/" {
                                current = current.parent.clone().expect("We should always have parent, otherwise I program badly");
                            }
                        }
                    },
                    "ls" => (),
                    _ => panic!(),
                }
            } else if parts[0] != "dir" {
                let current_size: u64 = parts[0].parse::<u64>().unwrap();
                Rc::clone(&current).children.borrow_mut().push(
                    Rc::new(FsDesc{
                        parent: Some(Rc::clone(&current)),
                        name: parts[1].to_string(),
                        size: current_size.into(),
                        children: Vec::new().into(),
                }));
            } else {
                assert_eq!(parts[0],"dir");
                Rc::clone(&current).children.borrow_mut().push(
                    Rc::new(FsDesc{
                        parent: Some(Rc::clone(&current)),
                        name: parts[1].to_string(),
                        size: 0.into(),
                        children: Vec::new().into(),
                }));
            }
        }
    }

    println!("");
    println!("The current tree");
    return recurse_set_sizes(root.clone(), 100000);
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
        let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";
        assert_eq!(process_input(input),95437);
    }
}
