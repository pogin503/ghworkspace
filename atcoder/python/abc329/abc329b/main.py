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
    ss = li_in()
    ss_set = set(ss)
    ans = sorted(list(ss_set))[len(ss_set) - 2]
    print(ans)


def main():
    solve1()


if __name__ == '__main__':
    main()
