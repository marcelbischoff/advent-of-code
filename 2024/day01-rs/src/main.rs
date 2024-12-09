use std::fs;

fn get_sorted(input: &str) -> (Vec<i32>, Vec<i32>) {
    let parts = input.lines().map(|s| {
        let a = s
            .split_whitespace()
            .map(|t| t.parse::<i32>().unwrap())
            .collect::<Vec<i32>>();
        (a[0], a[1])
    });

    let (mut first, mut second): (Vec<i32>, Vec<i32>) = parts.unzip();
    first.sort();
    second.sort();
    (first, second)
}

fn solve_part1(first: Vec<i32>, second: Vec<i32>) -> i32 {
    first
        .iter()
        .zip(second.iter())
        .map(|arr| (arr.0 - arr.1).abs())
        .sum()
}

fn main() {
    let input = fs::read_to_string("../day01/input.txt").expect("file not found");
    let (first, second): (Vec<i32>, Vec<i32>) = get_sorted(&input);

    let result = solve_part1(first, second);

    println!("Result: {result}");
}
