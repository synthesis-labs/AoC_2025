import re

file = 'data/test.txt'
# file = 'data/input.txt'

with open(file, 'r') as infile:
    data = infile.read().split('\n')
