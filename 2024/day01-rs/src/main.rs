use std::fs;

fn get_sorted(input: &str) -> (Vec<i32>, Vec<i32>) {
    let parts = input.lines().filter_map(|s| {
        let a = s
            .split_whitespace()
            .map(|t| t.parse::<i32>().unwrap())
            .collect::<Vec<i32>>();
        match a[..] {
            [x, y] => Some((x, y)),
            _ => None,
        }
    });

    let (mut first, mut second): (Vec<i32>, Vec<i32>) = parts.unzip();
    first.sort();
    second.sort();
    (first, second)
}

fn solve_part1(first: &Vec<i32>, second: &Vec<i32>) -> i32 {
    first
        .iter()
        .zip(second.iter())
        .map(|arr| (arr.0 - arr.1).abs())
        .sum()
}
fn multiplicity(second: Vec<i32>, i: i32) -> i32 {
    second.iter().filter_map(|n| match n {
        x if *x== i => Some(1),
        _ => None,
    }).sum()
}

fn solve_part2(first: &Vec<i32>, second: &Vec<i32>) -> i32 {
    first
        .iter()
        .map(|i| i * multiplicity(second.clone(), *i))
        .sum()
}

fn main() {
    let sample = fs::read_to_string("../day01/sample.txt").expect("file not found");
    let (first, second): (Vec<i32>, Vec<i32>) = get_sorted(&sample);
    assert!(solve_part1(&first, &second) == 11);
    assert!(solve_part2(&first, &second) == 31);

    let input = fs::read_to_string("../day01/input.txt").expect("file not found");
    let (first, second): (Vec<i32>, Vec<i32>) = get_sorted(&input);
    let result = solve_part1(&first, &second);
    println!("result: {result}");
    let result = solve_part2(&first, &second);
    println!("result: {result}");
}
