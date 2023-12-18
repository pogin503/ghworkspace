#!/usr/bin/env python
# coding: utf-8
import sys


def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n, m = li_in()
    s = s_in()
    t = s_in()
    t_s_with = t.startswith(s)
    t_e_with = t.endswith(s)
    ans = -1
    if t_s_with == True and t_e_with == True:
        ans = 0
    elif t_s_with == True:
        ans = 1
    elif t_e_with == True:
        ans = 2
    else:
        ans = 3
    print(ans)
    # s = [ input() for i in range(N)]


def main():
    solve1()

if __name__ == '__main__':
    main()
