#!/usr/bin/python
#-*- coding: UTF-8 -*-
# 2007.4.24
#
# エラトステネスの篩による素数表作成プログラム

from math import *	# 数学関数を使うためのまじない
from time import *	# 時間関数を使うためのまじない
from sys import *	# システム関数を使うためのまじない

print '素数表をつくります。'
print 'いくつまでの素数をしらべますか。'
max = input()

t0 = time()		# 開始時刻
sq = sqrt(max)
t = range(0,max+1)
t[0] = 0
t[1] = 0
p = 2
while p <= sq:
	if t[p] != 0:
		j = p + p
		while j <= max:
			t[j] = 0
			j += p
	p += 1
t1 =  time()		# 終了時刻
c = 0
print
for p in t:
	if p != 0:
		print p,
		c += 1
print
print max, 'までの素数の個数 = ',c
print '計算時間 =', t1 - t0
fin = stdin.readline()	# Enter キーで終了
