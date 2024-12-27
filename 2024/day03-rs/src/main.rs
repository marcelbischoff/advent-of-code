use std::fs;

fn compute(input: &String) -> u32 {
    let mut i: usize = 0;
    let mut result: u32 = 0;
    while i < input.len() - 4 {
        match &input[i..(i + 4)] {
            "mul(" => {
                i += 4;
                let comma_splitted = &input[i..].split(",").collect::<Vec<&str>>();
                if comma_splitted.len() > 1 {
                    let first = comma_splitted[0].parse::<u32>();
                    let second =
                        comma_splitted[1].split(")").collect::<Vec<&str>>()[0].parse::<u32>();
                    match (first, second) {
                        (Ok(a), Ok(b)) => {
                            if 0 < a && a < 1000 && 0 < b && b < 1000 {
                                result += a * b;
                            }
                        }
                        _ => (),
                    }
                }
            }
            _ => {
                i += 1;
            }
        }
    }
    result
}

fn main() {
    let sample = fs::read_to_string("sample.txt").expect("file not found");
    let result = compute(&sample);
    assert!(result == 161);
    let sample = fs::read_to_string("input.txt").expect("file not found");
    let result = compute(&sample);
    println!("result: {result}");
}
