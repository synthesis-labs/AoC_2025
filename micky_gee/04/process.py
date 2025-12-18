import re
import numpy as np
from scipy.signal import convolve2d


# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')

data = np.array([[1 if x == '@' else 0 for x in list(line)] for line in data])

window = np.ones((3,3))

# result = convolve2d(data, window, 'same') * data

# answer = np.sum((result <= 4) * (data))

# print(answer)

total = 0
removed = 1

while removed > 0:
    result = convolve2d(data, window, 'same') * data
    to_remove = (result <= 4) * (data)
    removed = np.sum(to_remove)
    total += removed
    data = data - to_remove