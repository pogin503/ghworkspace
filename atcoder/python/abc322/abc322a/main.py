#!/usr/bin/env python
# coding: utf-8
import sys


def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_is(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n = i_in()
    xs = s_in()

    ans = -1
    for i in range(n - 2):
        if xs[i] == 'A' and xs[i + 1] == 'B' and xs[i + 2] == 'C':
            ans = i + 1
            break
    print(ans)


def main():
    solve1()


if __name__ == '__main__':
    main()
