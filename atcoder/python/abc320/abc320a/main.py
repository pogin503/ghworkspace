#!/usr/bin/env python
# coding: utf-8
import sys


def i(): return int(sys.stdin.readline().rstrip())
def li(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s(): return sys.stdin.readline().rstrip()
def ls(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    a,b = li()
    print(pow(a, b) + pow(b, a))


def main():
    solve1()


if __name__ == '__main__':
    main()
