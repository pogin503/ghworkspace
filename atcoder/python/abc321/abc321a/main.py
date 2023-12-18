#!/usr/bin/env python
# coding: utf-8
import sys


def i(): return int(sys.stdin.readline().rstrip())
def li(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s(): return sys.stdin.readline().rstrip()
def ls(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n = list(map(int, sys.stdin.readline().rstrip()))
    ans = True
    for i in range(len(n) - 1):
        if n[i] <= n[i + 1]:
            ans = False
            break
    if ans:
        print("Yes")
    else:
        print("No")


def main():
    solve1()


if __name__ == '__main__':
    main()
