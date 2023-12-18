#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def judge(l, r, a) -> int :
    if l < a and a < r:
        return a
    elif a < r:
        return l
    else:
        return r

def solve1():
    n, l, r = li_in()
    ary = li_in()
    s_ary = [0 for _ in range(n)]
    for i in range(n):
        s_ary[i] = judge(l, r, ary[i])
    print(*s_ary)


def main():
    solve1()


if __name__ == '__main__':
    main()
