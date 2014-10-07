import time, random, numpy as np

def perceptron(pa, w, n):
    counter = 0
    while True:
        pa[:, 4] = -1.0
        pa[pa[:, 0]*w[0] + pa[:, 1]*w[1] + w[2] >= 0, 4] = 1.0
        misConfig = np.arange(0, n)[pa[:, 3] != pa[:, 4]]
        if not misConfig.size:
            break
        i = random.choice(misConfig)
        w += np.array([pa[i,3] * 3]) * np.array([pa[i, 0], pa[i, 1], 1])
        counter += 1
    return counter


tt = time.time()
n, N = 100, 1000
pa = np.zeros((n, 5), float)
Counter, areaTotal, areaCount = 0, 0, 0
for uberCount in range(N):
    pa[:, 0:2] = np.random.uniform(-1.0, 1.0, (n, 2))
    x1, y1 = -1.0, random.uniform(-1.0, 1.0)
    x2, y2 = 1.0, random.uniform(-1.0, 1.0)
    b = (y2 + y1)/2.0
    a = (y2 - y1)/2.0
    f = (a, b, (x1, x2), (y1, y2))
    pa[:, 3] = -1.0
    pa[pa[:, 1] >= f[0]*pa[:, 0] + f[1], 3] = 1.0
    w = np.zeros(3)
    Counter += perceptron(pa, w, n)
    if w.any():  # if a vector of weights was generated
        error = 0
        ga = -w[0]/w[1]  # work out a & b for y = a*x + b
        gb = -w[2]/w[1]
        ma = np.random.uniform(-1.0, 1.0, (n, 2))
        for i in range(n):  # get random x and y from array ma
            if ((ma[i, 1] >= f[0] * ma[i, 0] + f[1]) and
                    (ma[i, 1] < ga * ma[i, 0] + gb)):
                error += 1  # add one error for different answers
        areaTotal += (error / n)  # probability = error over number of points
        areaCount += 1
print('Time taken was: %.3f' % (time.time() - tt))
