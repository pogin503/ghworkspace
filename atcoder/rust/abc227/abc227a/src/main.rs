use proconio::input;

fn solve1() {
    input!{
        n: i64,
        k: i64,
        a: i64,
    }

    let ans = if (a + k - 1) % n == 0 {
        n
    } else {
        (a + k - 1) % n
    };
    println!("{}", ans);

}

fn main() {
    solve1();
}
