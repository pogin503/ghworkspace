#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def calc(s):
    if s == 'AB' or s == 'BC' or s == 'CD' or s == 'DE' or s == 'AE':
        return 1
    elif s == 'AC' or s == 'BD' or s == 'CE' or s == 'AD' or s == 'BE':
        return 2
    else:
        raise

def solve1():
    s = s_in()
    t = s_in()
    if s[0] > s[1]:
        s1 = s[1] + s[0]
    else:
        s1 = s
    if t[0] > t[1]:
        t1 = t[1] + t[0]
    else:
        t1 = t
    ans = calc(s1) == calc(t1)
    
    print("Yes" if ans else "No")


def main():
    solve1()


if __name__ == '__main__':
    main()
