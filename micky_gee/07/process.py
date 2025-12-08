import re
import numpy as np
from scipy.signal import convolve2d

np.set_printoptions(linewidth=370)

file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')

data = np.array([[0 if x == '.' else 1 for x in line] for line in data])

beam = data[0]
print(beam)
window = np.array([1, 0, 1])

split_count = 0
for x in data[1:]:
    #find activated splitters
    b2 = 1*(beam > 0) * x * beam
    #generate the splitting pattern
    splits = np.convolve(b2, window, 'same')
    #propegate through freespace
    freespace = (x != 1) * 1
    beam = (beam + splits)*freespace
    print(f'{beam}\t{x}\t{b2}\t{splits}\t{sum(beam)}-{sum(b2)}')
    # print(beam)
    #count splits
    split_count += sum(b2)
    # print(splits)

print(f'Total splits: {split_count}')