import re

# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')
    data = [[int(x) for x in line] for line in data]


# total = 0
# for line in data:
#     first_index = max(range(len(line)-1), key=lambda i: line[i])
#     second_index = max(range(first_index + 1, len(line)), key=lambda i: line[i])
#     print(f'First index: {first_index} is {line[first_index]} and second index: {second_index} is {line[second_index]}')
#     print(line)
#     total += 10*line[first_index] + line[second_index]


total = 0
batteries = 12
for line in data:
    joltage = 0
    index = 0
    for i in range(batteries, 0, -1):
        index = max(range(index, len(line)-i+1), key=lambda i: line[i])
        joltage += line[index]*(10**(i-1))
        print(f'{joltage} after adding {line[index]} at index {index}')
        index += 1
    total += joltage
        
    # print(f'First index: {first_index} is {line[first_index]} and second index: {second_index} is {line[second_index]}')
    # print(line)
    # total += 10*line[first_index] + line[second_index]