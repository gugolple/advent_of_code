use std::io;
use std::io::prelude::*;
use std::collections::*;
use std::ops::Add;

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
struct Point3d {
    x: i64,
    y: i64,
    z: i64,
}

impl Add for Point3d {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

fn input_to_coll(input: &str) -> Vec<Point3d> {
    let mut res: Vec<_> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            println!("Line!: {}", line);
            let points = line.split(",").map(|x| x.parse::<i64>().unwrap()).collect::<Vec<i64>>();
            res.push(Point3d{
                x: points[0],
                y: points[1],
                z: points[2],
            });
        }
    }
    return res;
}

fn process_input(input: &str) -> u64 {
    let coll: HashSet<Point3d> = HashSet::from_iter(input_to_coll(input).into_iter());
    println!("{:?}", coll);
    let minx = coll.iter().map(|x| x.x).min().unwrap()-1;
    let miny = coll.iter().map(|x| x.y).min().unwrap()-1;
    let minz = coll.iter().map(|x| x.z).min().unwrap()-1;
    let maxx = coll.iter().map(|x| x.x).max().unwrap()+1;
    let maxy = coll.iter().map(|x| x.y).max().unwrap()+1;
    let maxz = coll.iter().map(|x| x.z).max().unwrap()+1;

    println!("minx {}", minx);
    println!("miny {}", miny);
    println!("minz {}", minz);
    println!("maxx {}", maxx);
    println!("maxy {}", maxy);
    println!("maxz {}", maxz);
    let mut seen: HashSet<Point3d> = HashSet::new();
    let mut pending: VecDeque<Point3d> = VecDeque::new();
    pending.push_back(Point3d{x: minx, y: miny, z: minz});

    let mut total_faces = 0;
    const next_pos: [Point3d;6] = [
        Point3d{x: -1, y: 0, z: 0},
        Point3d{x: 1, y: 0, z: 0},
        Point3d{x: 0, y: -1, z: 0},
        Point3d{x: 0, y: 1, z: 0},
        Point3d{x: 0, y: 0, z: -1},
        Point3d{x: 0, y: 0, z: 1},
    ];
    while pending.len() > 0 {
        let cur = pending.pop_front().unwrap();
        if cur.x < minx || cur.y < miny || cur.z < minz ||
           cur.x > maxx || cur.y > maxy || cur.z > maxz {
               continue;
        }
        if seen.contains(&cur) {
            continue;
        }
        if coll.contains(&cur) {
            total_faces = total_faces + 1;
            continue;
        }
        for np in &next_pos {
            pending.push_back(*np + cur);
        }
        seen.insert(cur);
    }
    return total_faces.try_into().unwrap();
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
    fn basic() {
        let input = "1,1,1
2,1,1";
        assert_eq!(process_input(input),10);
    }

    #[test]
    fn basic1() {
        let input = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";
        assert_eq!(process_input(input),58);
    }
}
