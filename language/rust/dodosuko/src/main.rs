use rand::Rng;

fn main() {
    let ddsk = vec!["ドド", "スコ"];
    let ans : &'static str = "ドドスコスコスコドドスコスコスコドドスコスコスコ";
    let mut rng = rand::thread_rng(); // デフォルトの乱数生成器を初期化
    let mut xs = vec![];
    for _i in 0..11 {
        xs.push(ddsk[rng.gen_range(0..2)]);
    }
    print!("{}", xs.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(""));
    for _x in 0..3000 {
        let choice = ddsk[rng.gen_range(0..2)];
        xs.push(choice);
        let xs_str = xs.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("");
        if ans == xs_str {
            println!("{}", choice);
            println!("ラブ注入♡");
            break;
        } else {
            print!("{}", choice);
            xs.remove(0);
        }
    }
}
