#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    ns = s_in()
    n = int(ns)
    for x in range(n, 920):
        digits = [int(x) for x in str(x)]
        if digits[0] * digits[1] == digits[2]:
            print(x)
            break


def main():
    solve1()


if __name__ == '__main__':
    main()
