import re

# file = 'data/test.txt'
file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split(',')

data = [re.findall(r'(\d+)-(\d+)', x)[0] for x in data]

sum = 0
for idrange in data:
    start = int(idrange[0])
    end = int(idrange[1])
    total = 0
    for number in range(start, end + 1):
        # digits = [int(x) for x in str(number)]
        # if len(str(number))%2 != 0:
        #     continue

        # print(number)

        for factors in range(1, len(str(number))):
            # print(f'  factors: {factors}')
            if len(str(number)) % factors != 0:
                continue

            substrings = [str(number)[i:i+factors] for i in range(0, len(str(number)), factors)]

        #     #check if all substrings are the same
            if all(x == substrings[0] for x in substrings):
                total += 1
                sum += number
                print(f'    found invalid ID: {number}')
                break

        # part 1
        # # if str(number)[:int(len(str(number))/2)] == str(number)[int(len(str(number))/2):]:
        # #     total += 1
        # #     sum += number

    print(f'From {start} to {end} there are {total} invalid IDs.')

        