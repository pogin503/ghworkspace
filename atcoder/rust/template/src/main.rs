#![allow(unused_imports)]
// #![allow(unused_variables)]
#![allow(dead_code)]
use proconio::input;
// use proconio::marker::*;
use std::cmp::{min, max, Reverse};
use std::collections::{HashSet, HashMap, BinaryHeap, VecDeque};

// static MOD: i64 = 1e9 as i64 + 7;
static MOD: u64 = 998244353;

fn mod_pow(x: u64, y: u64)->u64{
    if y == 0 {
        return 1;
    }
    let mut res = mod_pow(x * x % MOD, y / 2);
    if y % 2 == 1 {
        res = res * x % MOD;
    }
    res
}

fn solve1() {
    input!{
        n: i64
    }
    let ans : i64 = n;
    println!("{}", ans);

}

fn main() {
    solve1();
}
