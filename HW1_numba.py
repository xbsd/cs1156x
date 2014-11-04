import numpy as np
import matplotlib.pyplot as plt
import random
import time
from numba import jit, njit
 
 
def plotWithLines(pa, f, g):
    pb = pa.copy()
    pc = pa.copy()
    for i in range(len(pa)):
        if pa[i, 1] >= f[0]*pa[i, 0] + f[1]:
            pc[i] = (np.NaN, np.NaN, np.NaN, np.NaN, np.NaN)
        else:
            pb[i] = (np.NaN, np.NaN, np.NaN, np.NaN, np.NaN)
    plt.figure(1)
    len(pb) and plt.plot(pb[:, 0], pb[:, 1], 'bo')
    len(pc) and plt.plot(pc[:, 0], pc[:, 1], 'ro')
    plt.ylim([-1, 1])
    plt.plot(f[2], f[3], '')
    plt.plot(g[2], g[3], 'g')
    plt.show()
 
 
@njit
def applyg(pa, w, n):
    '''
   Apply function g to x and y
   Numba is applied as a decorator
     jit is not strict, njit is
   Note that numba converts simple loops
     to run at C speed so clever ufuncs
     are abandoned
   '''
    a, b, c = w[0], w[1], w[2]
    for i in range(n):
        if pa[i, 0] * a + pa[i, 1] * b + c >= 0:
            pa[i, 4] = 1
        else:
            pa[i, 4] = -1
           
@njit
def upW(pa, w, counter, misConfig):
    '''
   Perceptron: Adjust the Weights for a misConfiguration
   My Numba type casting skill is weak
   In this case internal casts to int resulted
     in a slight speed up
   '''
    i = int(misConfig[(counter+3) % len(misConfig)])
    j = int(pa[i, 3])
    w[0] += j * pa[i, 0]
    w[1] += j * pa[i, 1]
    w[2] += j
    return counter + 1
   
 
def perceptron(pa, w, n, selectOne):
    '''
   Perceptron: the main function that applies g
     calculates the misConfigurations
     adjusts the weights by one of the misConfigs
     (originally chosen with random.choice but its
     faster to choose based on the loop count)
   '''
    counter = 0
    misConfig = np.array([1])
    while misConfig.size:
        applyg(pa, w, n)
        misConfig = selectOne[pa[:, 3] != pa[:, 4]]
        if misConfig.size:
            counter = upW(pa, w, counter, misConfig)
    return counter
 
 
def percepTest(n, N, ma, selectOne):
    '''
   Run the test, looping N times
     1 Build the array of points, pa
     2 Generate a line f at random
     3 Apply f to mark the points + or -
     4 Apply perceptron to create g and counter
     5 Apply montecarlo to test g vs f error
       Note that for 1000 loops, only 5 samples are
         taken per loop as required accuracy is low
   '''
    Counter, areaTotal, areaCount = 0, 0, 0
    for uberCount in range(N):
        # 1
        pa[:, 0:2] = np.random.uniform(-1.0, 1.0, (n, 2))
        # pa[:, 2] = 1  # eliminated constant multiplier for speed
        # 2
        x1, y1 = -1.0, random.uniform(-1.0, 1.0)
        x2, y2 = 1.0, random.uniform(-1.0, 1.0)
        b = (y2 + y1)/2.0
        a = (y2 - y1)/2.0
        f = (a, b, (x1, x2), (y1, y2))
        # 3
        pa[:, 3] = -1.0
        pa[pa[:, 1] >= f[0]*pa[:, 0] + f[1], 3] = 1.0
        # 4
        w = np.zeros(3)
        Counter += perceptron(pa, w, n, selectOne)
        # 5
        if w.any():  # if a vector of weights was generated
            error = 0
            ga = -w[0]/w[1]  # work out a & b for y = a*x + b
            gb = -w[2]/w[1]
            for i in range(mc):  # get random x and y from array ma
                # test if f and g give the same answer
                if ((ma[i, 1] >= f[0] * ma[i, 0] + f[1]) and
                        (ma[i, 1] < ga * ma[i, 0] + gb)):
                    error += 1  # add one error for different answers
            areaTotal += (error / mc)  # probability = error over number of points
            areaCount += 1
    # g = 0, 0, (-1.0, 1.0), (((w[0] - w[2]) / w[1]), (-(w[0] + w[2]) / w[1]))
    # plotWithLines(pa, f, g)
    return w, Counter, (areaCount and areaTotal/areaCount)
 
 
tt = time.time()
n, N = 100, 1000
mc = int(4000 / N)
ma = np.random.uniform(-1.0, 1.0, (10000, 2))
selectOne = np.arange(0, n)
pa = np.zeros((n, 5), float)
w, superCounter, superArea = percepTest(n, N, ma, selectOne)
print('.. counter = %.0f' % (superCounter / float(N)))
print('.. area = %.4f' % superArea)
print('\n', w, '\n')
print('Time taken was: %.3f' % (time.time() - tt))
