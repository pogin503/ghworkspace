#!/usr/bin/env python
# coding: utf-8
import sys

def i(): return int(sys.stdin.readline().rstrip())
def li(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s(): return sys.stdin.readline().rstrip()
def ls(): return list(sys.stdin.readline().rstrip().split())


def is_palindrome(str):
    judge1 = True
    mid = int(len(str) / 2)
    for i in range(mid):
        if str[i] != str[len(str) - i - 1]:
            judge1 = False
            break
    return judge1


def solve1():
    s = sys.stdin.readline().rstrip()
    length = len(s)
    ans = 1
    for x in range(length - 1):
        step_len = len(s[x:length])
        for y1 in range(length - step_len + 1):
            if is_palindrome(s[y1:step_len+y1]) == True:
                ans = step_len
        if ans != 1:
            break
    print(ans)

def main():
    solve1()


if __name__ == '__main__':
    main()
