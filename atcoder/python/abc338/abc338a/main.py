#!/usr/bin/env python
# coding: utf-8
import sys
import itertools
import re

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def cap(s):
    if len(s) == 1:
        if re.match("[A-Z]", s):
            return True
    else:
        ret1 = re.match("[A-Z]", s[0])
        ret2 = re.match("[a-z]+$", s[1:])
        if ret1 != None and ret2 != None:
            return True
    return False


def solve1():
    s = s_in()
    ans = cap(s)
    
    if ans:
        print("Yes")
    else: 
        print("No")
        
    # s = [ input() for i in range(N)]


def main():
    solve1()


if __name__ == '__main__':
    main()
