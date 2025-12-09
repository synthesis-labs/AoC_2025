import re
import numpy as np
import pprint
import itertools

# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')

operators = data[-1].split()
# data = [list(map(int, re.findall(r'\d+', line))) for line in data[:-1] if line]

# # print(data)
# #transpose data:
# data = list(map(list, zip(*data)))
# # print(data)

# results = []
# for op, vals in zip(operators, data):
#     if op == '+':
#         result = np.sum(vals)
#     else:
#         result = np.prod(vals)
#     results.append(result)

# # print(results)

# data = [list(map(lambda x: 0 if x == ' ' else int(x), re.findall(r'[\d|\s]', line))) for line in data[:-1] if line]
data = [list(map(lambda x: x, re.findall(r'[\d|\s]', line))) for line in data[:-1] if line]
data = list(map(list, zip(*data)))
groups = [i for i, x in enumerate(data) if x == [' ', ' ', ' ', ' ']]
groups = [-1] + groups + [len(data)]
pprint.pprint(data)
pprint.pprint(groups)
pprint.pprint([(x+1,y) for x,y in itertools.pairwise(groups)])
groups = [(x+1,y) for x,y in itertools.pairwise(groups)]

results = []
for op, indices in zip(operators, groups):
    # print(op)
    print(f'data is: {data[indices[0]:indices[1]]}')
    vals = [int(''.join(x)) for x in data[indices[0]:indices[1]]]
    print(vals)
    if op == '+':
        result = np.sum(vals)
    else:
        result = np.prod(vals)
    results.append(result)
    
