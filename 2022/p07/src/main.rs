use std::io;
use std::io::prelude::*;
use std::collections::HashMap;


struct FsDesc {
    name: String,
    size: u64,
    child: Vec<String>,
    visited: bool
}

fn recurse_add_size_folders(input: &str, hm: &mut HashMap<String, FsDesc>) {
    let elem = hm.get(input).expect("Potato!");
    if elem.visited {
        return;
    }

    let mut tot: u64 = 0;
    for chl in elem.child.clone().iter() {
        recurse_add_size_folders(chl, hm);
        let next_child = hm.get(chl).expect("Int Potato!");
        tot = tot + next_child.size;
    }
    let elem = hm.get_mut(input).expect("Potato!");
    elem.size = tot + elem.size;
}

fn add_sizes_folders(input: &mut HashMap<String, FsDesc>, thresh: u64) -> u64 {
    recurse_add_size_folders("root", input);

    let mut tot: u64 = 0;
    for (key, value) in input {
        println!("Key: {}, val: {}", key, value.size);
        if value.size < thresh {
            tot = tot + value.size;
        }
    }
    return tot;
}

fn process_input(input: &str) -> u64 {
    let mut path: Vec<String> = Vec::new();
    let mut folder_tree: HashMap<String, FsDesc> = HashMap::new();
    let mut folder_size: u64 = 0;
    let mut fold_name = "root".to_string();
    let mut fold_children: Vec<FsDesc> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            //println!("");
            //println!("Line!: {}", line);
            //println!("Path!: {:?}", path);
            let parts: Vec<_> = line.split(" ").collect();
            if parts[0] == "$" {
                match parts[1] {
                    "cd" => {
                        println!("Path: {}", &path.join("/"));
                        if parts[2] != ".." || folder_size>0 {
                            folder_tree.insert(
                                path.join("/"),
                                FsDesc {
                                    name: fold_name.clone(),
                                    size: folder_size,
                                    child: Vec::new(),
                                    visited: false
                                });
                            if path.len() > 1 {
                                println!("paatthh {:?} {}", path, &path.split_last().unwrap().1.join("/"));
                                let tfs = folder_tree.get_mut(&path.split_last().unwrap().1.join("/"));
                                tfs.unwrap().child.push(path.join("/"));
                            }
                        }
                        if parts[2] == "/" {
                            path.clear();
                            path.push("root".to_string());
                        }else {

                            if parts[2] == ".." {
                                path.pop().expect("Will work, the people are nice");
                            }else{
                                path.push(parts[2].to_string());
                            }
                        }
                        fold_name = parts[2].to_string();
                        folder_size = 0;
                    },
                    "ls" => (),
                    _ => panic!(),
                }
            } else if parts[0] != "dir" {
                folder_size = folder_size + parts[0].parse::<u64>().unwrap();
            } else {
                assert_eq!(parts[0],"dir");
                fold_children.push(FsDesc{
                    name: parts[1],
                    size: 0,

                });
            }
        }
    }
    if folder_size > 0 {
        folder_tree.insert(
            path.join("/"),
            FsDesc {
                name: fold_name,
                size: folder_size,
                child: Vec::new(),
                visited: false
            });
        if path.len() > 1 {
            let tfs = folder_tree.get_mut(&path.split_last().unwrap().1.join("/"));
            tfs.unwrap().child.push(path.join("/"));
        }
    }

    for (key, value) in &folder_tree {
        println!("Key: {}, Name: {}, Size: {}, Cihld: {}", key, value.name, value.size, value.child.len());
        for chl in &value.child {
            println!("  Child: {}", chl);
        }
    }

    return add_sizes_folders(&mut folder_tree, 100000);
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

    #[test]
    fn test_advent_basic1() {
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
