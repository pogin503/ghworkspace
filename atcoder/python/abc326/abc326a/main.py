#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    x, y = li_in()
    sub = y - x

    ans = ""
    if sub >= -3 and sub <= 2:
        ans = "Yes"
    else:
        ans = "No"
    # s = [ input() for i in range(N)]
    print(ans)

def main():
    solve1()


if __name__ == '__main__':
    main()
