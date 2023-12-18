#!/usr/bin/env python
# coding: utf-8
import sys


def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n = i_in()
    xs = [None] * n
    for i in range(n):
        xs[i] = s_in()
    xs_score = [0] * n
    for i in range(len(xs)):
        for j in range(len(xs[i])):
            if xs[i][j] == 'o':
                xs_score[i] = xs_score[i] + 1
    xs1 = zip(range(1, n + 1), xs_score)
    xs_sorted = sorted(xs1, key=lambda x: x[1], reverse=True)
    # print(list(map(lambda x: x[0], xs_sorted)))
    print(" ".join(list(map(lambda x: str(x[0]), xs_sorted))))
        
    # s = [ input() for i in range(N)]


def main():
    solve1()


if __name__ == '__main__':
    main()
