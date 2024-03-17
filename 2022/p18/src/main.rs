use std::io;
use std::io::prelude::*;

#[derive(Debug)]
struct Point3d {
    x: i64,
    y: i64,
    z: i64,
}

fn distance(left: &Point3d, right: &Point3d) -> i64 {
    (left.x - right.x).abs() + 
    (left.y - right.y).abs() + 
    (left.z - right.z).abs()
}

fn connected_side(left: &Point3d, right: &Point3d) -> bool {
    distance(left, right) == 1
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
    let coll = input_to_coll(input);
    println!("{:?}", coll);

    let mut connected_sides = 0;
    for itr1 in coll.iter() {
        connected_sides = connected_sides + coll.iter().map(|x| connected_side(x, itr1)).filter(|x| *x).count();
    }

    return (6*coll.len() - connected_sides).try_into().unwrap();
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
        assert_eq!(process_input(input),64);
    }
}
