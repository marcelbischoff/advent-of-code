use std::fs;

fn parse_lines(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|s| {
            s.split_whitespace()
                .map(|t| t.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>()
}

enum State {
    Undetermined,
    Inc,
    Dec,
    NotSafe,
}

fn check(a: i32, b: i32) -> bool {
    (b > a) && (b < a + 4)
}

fn first_state(previous: i32, current: i32) -> State {
    if (previous < current) && check(previous, current) {
        return State::Inc;
    }
    if (current < previous) && check(current, previous) {
        return State::Dec;
    }
    State::NotSafe
}

fn is_safe(line: Vec<i32>) -> i32 {
    let (_, res) = line.into_iter().fold(
        (None, State::Undetermined),
        |(prior, state), current: i32| match (prior, state) {
            (None, State::Undetermined) => (Some(current), State::Undetermined),
            (Some(previous), State::Undetermined) => {
                (Some(current), first_state(previous, current))
            }
            (Some(previous), State::Inc) => {
                if check(previous, current) {
                    (Some(current), State::Inc)
                } else {
                    (None, State::NotSafe)
                }
            }
            (Some(previous), State::Dec) => {
                if check(current, previous) {
                    (Some(current), State::Dec)
                } else {
                    (None, State::NotSafe)
                }
            }
            _ => (None, State::NotSafe),
        },
    );
    match res {
        State::Inc => 1,
        State::Dec => 1,
        _ => 0,
    }
}

fn is_safe_two(line: Vec<i32>) -> i32 {
    if is_safe(line.clone()) == 1 {
        return 1;
    };
    if (0..(line.len()))
        .map(|i| {
            line.clone()
                .into_iter()
                .enumerate()
                .filter_map(|(j, s)| {
                    if i as i32 != j.try_into().unwrap() {
                        Some(s)
                    } else {
                        None
                    }
                })
                .collect()
        })
        .collect::<Vec<Vec<i32>>>()
        .iter()
        .any(|line| is_safe(line.to_vec()) > 0)
    {
        return 1;
    };
    0
}

fn main() {
    let sample = fs::read_to_string("../day02/sample.txt").expect("file not found");
    let sample_lines: Vec<Vec<i32>> = parse_lines(&sample);
    assert!(
        sample_lines
            .clone()
            .into_iter()
            .map(is_safe)
            .into_iter()
            .sum::<i32>()
            == 2
    );

    let input = fs::read_to_string("../day02/input.txt").expect("file not found");
    let lines: Vec<Vec<i32>> = parse_lines(&input);
    let result: i32 = lines.clone().into_iter().map(is_safe).into_iter().sum();
    assert!(result == 524);

    assert!(
        sample_lines
            .into_iter()
            .map(is_safe_two)
            .into_iter()
            .sum::<i32>()
            == 4
    );
    let result: i32 = lines.clone().into_iter().map(is_safe_two).into_iter().sum();
    println!("result: {result}");
}
