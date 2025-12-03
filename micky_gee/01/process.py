import re
import numpy as np

# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')

# data = [x.split() for x in data]
    # r = re.findall(r'mul\((\d+),(\d+)\)', x)


data = [re.findall(r'([LR])(\d+)', x)[0] for x in data]

position = 50
count = 0

for combination in data:

    loops = int(combination[1]) // 100 #- (1 if position == 0 else 0)
    residual = int(combination[1]) % 100

    if combination[0] == 'R':
        position += residual
        loops += position // 100
        position = position % 100
        # if loops < 0:
        #     loops = 0

        print(f'loops: {loops}')
        # if position == 0:
        #     loops += 1
    else:
        newposition = (position - residual) % 100
        if newposition == 0:
            loops += 1
        elif newposition > position and position != 0:
            loops += 1
        else:
            pass
        position = newposition

    count += loops

#   position += int(combination[1])* (-1 if combination[0] == 'L' else 1)
#   shifts = abs(round(position // 100))
#   position = position % 100
#   count += shifts + (1 if position == 0 and shifts > 0 else 0)
    print(f'{combination} to {position} with {loops} shifts')
#   if position % 100 == 0:
#     count += 1