use std::io;
use std::io::prelude::*;

#[derive(Debug)]
struct Position {
    x: i64,
    y: i64,
}

#[derive(Debug)]
struct Range {
    start: i64,
    end: i64
}

const fn manhattan_distance(org: &Position, dst: &Position) -> i64 {
    (dst.x - org.x).abs() + (dst.y - org.y).abs()
}

fn overal_at_row(sensor: &Position, beacon: &Position, row: &i64) -> Option<Range> {
    let dist = manhattan_distance(sensor, beacon);
    let dist_at_row = manhattan_distance(sensor, &Position{x: sensor.x, y: *row});
    println!("Distance: {} DeltaDistance: {}", dist, dist_at_row);
    if dist_at_row <= dist {
        // + 1 to include the destination squares
        let delta = (dist - dist_at_row)/2 + 1;
        return Some(Range{start: sensor.x - delta, end: sensor.x + delta});
    }
    None
}

fn process_input(input: &str, row: i64) -> u64 {
    let mut sensors_beacons: Vec<(Position, Position)> = Vec::new();
    for line in input.split("\n") {
        if line != "" {
            println!("Line: {}", line);
            let sensor_beacon: Vec<_> = line.split(":").collect();
            let sensor: Vec<_> = sensor_beacon[0].split("at ").skip(1).take(1).collect::<Vec<_>>()[0].split(", ").map(|x| x.split("=").skip(1).take(1).collect::<Vec<_>>()[0].parse::<i64>().unwrap()).collect();
            let beacon: Vec<_> = sensor_beacon[1].split("at ").skip(1).take(1).collect::<Vec<_>>()[0].split(", ").map(|x| x.split("=").skip(1).take(1).collect::<Vec<_>>()[0].parse::<i64>().unwrap()).collect();
            sensors_beacons.push((
                    Position{
                        x: sensor[0],
                        y: sensor[1],
                    },
                    Position{
                        x: beacon[0],
                        y: beacon[1],
                    }
            ));
        }
    }

    let mut ranges: Vec<Range> = Vec::new();
    for (sen, bec) in sensors_beacons.iter() {
        println!("Sensor: {:?} Beacon: {:?}", sen, bec);
        if let Some(r) = overal_at_row(sen, bec, &row) {
            ranges.push(r);
        }
    }
    println!("");

    ranges.sort_by(|a, b| a.start.partial_cmp(&b.start).unwrap());
    let mut actual_ranges: Vec<Range> = Vec::new();
    let mut cur_range: Option<Range> = None;
    for ran in ranges.into_iter() {
        println!("PR: {:?}", ran);
        if cur_range.is_none() {
            cur_range = Some(ran);
        } else {
            if ran.start < cur_range.as_ref().expect("Has data").end {
                cur_range.as_mut().expect("Has data").end = ran.end;
            } else {
                actual_ranges.push(cur_range.expect("Already has data"));
                cur_range = Some(ran);
            }
        }
    }
    actual_ranges.push(cur_range.expect("Already has data"));
    println!("");

    let mut total = 0;
    for r in actual_ranges.iter() {
        println!("RR: {:?}", r);
        total = total + r.end - r.start + 1; // +1 to include
    }

    return total.try_into().unwrap();
}

fn main() -> io::Result<()> {
    // read into a String, so that you don't need to do the conversion.
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;

    println!("Total: {}", process_input(&buffer, 2000000));

    Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_advent_basic() {
        let input = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";
        assert_eq!(process_input(input, 10),26);
    }
}
