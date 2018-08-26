fn read<T: std::str::FromStr>() -> T {
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();
    s.trim().parse().ok().unwrap()
}

fn main() {
    let line: String = read();
    let mut cnt: i32 = 0;
    for x in 0..3 {
        if line.chars().nth(x).unwrap() == '1' {
            cnt += 1;
        }
    }
    println!("{}", cnt);
}
