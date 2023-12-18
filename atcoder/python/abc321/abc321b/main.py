#!/usr/bin/env python
# coding: utf-8
import sys
import copy

def i(): return int(sys.stdin.readline().rstrip())
def li(): return list(map(int, sys.stdin.readline().rstrip().split()))
def s(): return sys.stdin.readline().rstrip()
def ls(): return list(sys.stdin.readline().rstrip().split())


def solve1():
    n, x = list(map(int, input().split()))
    ax = list(map(int, input().split()))
    max_val = max(ax)
    min_val = min(ax)
    # list(map(lambda x: x != max_val and ))
    for i in range(len(ax)):
        if ax[i] == max_val:
            ax.pop(i)
            break
    if min_val != max_val:
        for i in range(len(ax)):
            if ax[i] == min_val:
                ax.pop(i)
                break    
    y = x - sum(ax)
    if 0 <= y and y <=100:
        print(y)
    else:
        print(-1)

# def solve1():
#     n, x = list(map(int, input().split()))
#     ax = list(map(int, input().split()))
#     ax.append(-1)
#     for last in range(0, 101):
#         bx = copy.copy(ax)
#         bx[-1] = last
#         bx.sort()
#         # list(map(lambda x: x != max_val and ))
#         y = sum(bx[1:len(bx) - 1])
#         if y >= x:
#             print(last)
#             return

#     print(-1)


def main():
    solve1()


if __name__ == '__main__':
    main()
