#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    s = s_in()
    # s = [ input() for i in range(N)]
    s1 = [s[i] for i in range(len(s)) if i % 2 != 0 ]
    ans = "Yes"
    for i in range(len(s1)):
        if s1[i] == '1':
            ans = "No"
    print(ans)


def main():
    solve1()


if __name__ == '__main__':
    main()
