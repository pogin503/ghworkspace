#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n = i_in()
    s = s_in()
    ans = "No"
    for i in range(n - 1):
        if (s[i] == 'a' and s[i + 1] == 'b') or (s[i] == 'b' and s[i + 1] == 'a'):
            ans = "Yes"
            break
    print(ans)
    # s = [ input() for i in range(N)]


def main():
    solve1()


if __name__ == '__main__':
    main()
