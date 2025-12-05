import re
from tqdm import tqdm
import itertools
import pprint

# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')

ranges = data[:data.index('')]
stock = data[data.index('')+1:]

# ranges = [(min(int(y[0][0]), int(y[0][1])), max(int(y[0][0]), int(y[0][1]))) for y in [re.findall(r'(\d+)-(\d+)', x) for x in ranges]]
ranges = [(int(y[0][0]), int(y[0][1])) for y in [re.findall(r'(\d+)-(\d+)', x) for x in ranges]]
stock = [int(x) for x in stock]

fresh = 0
for item in stock:
    if any(item in range(r[0], r[1]+1) for r in ranges):
        fresh += 1

ranges.sort(key=lambda x: x[0])
if all(range[0] <= range[1] for range in ranges):
    print('All ranges valid!')

old_ranges = ranges.copy()
# sets = set()
# for r in tqdm(ranges):
#     sets.union(set(range(r[0], r[1]+1)))

        # if (current[0] <= range[1] and current[0] >= range[0]) or (current[1] >= range[0] and current[1] <= range[1]):

changed = True
while changed:
    changed = False
    ranges.sort(key=lambda x: x[0])
    # print(ranges)
    for p in itertools.combinations(ranges, 2):
        current = p[0]
        range = p[1]
        if (current[0] <= range[1] and current[0] >= range[0]) or (current[1] >= range[0] and current[1] <= range[1]) or \
           (current[0] <= range[0] and current[1] >= range[1]) or (current[0] >= range[0] and current[1] <= range[1]):
            new_range = (min(current[0], range[0]), max(current[1], range[1]))
            if current in ranges and range in ranges:
                print(f'    Merging {current} and {range} into {new_range}')
                ranges.remove(current)
                ranges.remove(range)
                ranges.append(new_range)
                changed = True
    # print('.', end='', flush=True)
    print(f' Iteration complete, {len(ranges)} ranges remain.')

pprint.pprint(ranges)

x = [r[1] - r[0] + 1 for r in ranges]
