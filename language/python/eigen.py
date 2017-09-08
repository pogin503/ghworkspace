#!/usr/bin/env python
# coding=utf-8
import numpy

# a = numpy.array([[2,2]])
# # print a
# length = numpy.linalg.norm(a)
# print length
#length=>2.8284271247461903

#ベクトルの正規化
# print (a / numpy.linalg.norm(a))

def eigen_mul(A,x):
    return numpy.dot(A,x)

def eigen_div(A,x):
    A1 = eigen_mul(A,x)
    it = numpy.nditer(A1,flags=['f_index'])
    while not it.finished:
        A1[it.index] = it[0]/x[it.index]
        it.iternext()
    return A1

def eigen_min(A,x):
    A1 = eigen_div(A,x)
    return numpy.amin(A1)

def eigen_max(A,x):
    A1 = eigen_div(A,x)
    return numpy.amax(A1)

def eigen_tuple(A,x):
    # print A
    return (eigen_mul(A,x),eigen_min(A,x),eigen_max(A,x))

A = numpy.array([[3.0,2.0,1.0],[1.0,1.0,1.0],[2.0,2.0,1.0]])
A_1 = numpy.array([[1.0,1.0,1.0],[1.0,1.0,0.0],[1.0,0.0,1.0]])
x = numpy.array([1.0,1.0,1.0])
# print eigen_mul(A_1,x)
# print "A_1:",A_1
# print eigen_min(A_1,x)
# y_1 = eigen_mul(A_1,x)

def eigen_rad(A,x,output_range):
    x_local = x
    A1 = []
    for i in xrange(output_range):
        (y_i,a_min,a_max) = eigen_tuple(A,x_local)
        A1.append((y_i,a_min,a_max))
        x_local = y_i
    return A1

def print_eigen_tuple(tp_list):
    for i in xrange(len(tp_list)):
        print "tp[",i,"]=",tp_list[i]

# print eigen_rad(A_1,x,5)
print_eigen_tuple(eigen_rad(A,x,5))
# print eigen_rad(A,x,5)

# print tp
# print eigen_mul(A_1,numpy.array([3.0,2.0,2.0]))
# print eigen_min(A_1,numpy.array([3.0,2.0,2.0]))
# print (numpy.array([current[0]/prev[0],current[1]/prev[1],current[2]/prev[2]]))


# prev = a0 = numpy.array([[1.0],[1.0],[1.0]])
# current = numpy.dot(A,prev)

def get_eigen_diameter():
    for i in range(10):
        current = numpy.dot(A,current)
        print "current:",current
        y_i = numpy.array([current[0]/prev[0],current[1]/prev[1],current[2]/prev[2]])
        print "eigenvalue diameter:",y_i
        min1 = numpy.amin(y_i)
        max1 = numpy.amax(y_i)
        print min1, max1
        prev = numpy.dot(A,prev)


#=>array([[ 0.70710678,  0.70710678]])
