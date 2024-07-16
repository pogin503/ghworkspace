#!/usr/bin/env python
# coding: utf-8
import sys
import itertools

def i_in(): return int(sys.stdin.readline().rstrip())
def li_in(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s_in(): return sys.stdin.readline().rstrip()
def ls_in(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    s = s_in()
    # s = [ input() for i in range(N)]
    
    count_dic = {}
    for x in s:
        if count_dic.get(x):
            count_dic[x] += 1
        else:
            count_dic[x] = 1
    maxv = max(count_dic.values())
    
    kvlist = []
    for x in count_dic.items():
        if x[1] == maxv:
            kvlist.append(x)
    sorted1 = sorted(kvlist, key=lambda x: x[0])
    print(sorted1[0][0])


def main():
    solve1()


if __name__ == '__main__':
    main()
