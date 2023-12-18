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
    ds = ls_in()
    cnt = 0
    for mi in range(n):

        di1 = int(ds[mi])
        for di in range(di1):
            md = str(mi + 1) + str(di + 1)
            allt = True
            for i in range(1, len(md)):
                if md[0] != md[i]:
                    allt = False
                    break
            if allt:
                cnt = cnt + 1
    print(cnt)


def main():
    solve1()


if __name__ == '__main__':
    main()
