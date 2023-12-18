#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n, l = li_in()
    ary = li_in()
    print(len(list(filter(lambda x: x >= l, ary))))


def main():
    solve1()


if __name__ == '__main__':
    main()
