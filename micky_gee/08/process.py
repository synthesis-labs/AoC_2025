import re
import numpy as np

# file = 'data/test.txt'; n = 10
file = 'data/input.txt'; n = 1000

with open(file, 'r') as infile:
    data = infile.read().split('\n')

data = np.array([list(map(int, re.findall(r'(\d+),?', line))) for line in data])

#find pairwise distances between all points (1000x1000 is still tractable)
diff = data[:, np.newaxis, :] - data[np.newaxis, :, :]
pairwise_distances = np.linalg.norm(diff, axis=-1, ord=2)

b = np.unique(np.sort(pairwise_distances.flatten()))

connections= [np.where(pairwise_distances == x) for x in b[1:n+1]]
# connections= [np.where(pairwise_distances == x) for x in b][1:n+1]

#now connections contains the node-pairs of the shortest distances.  We've got to build a graph from these.

circuits = []
for link in connections:
    for c in circuits:
        if link[0][0] in c and link[1][0] not in c:
            # c.update([link[0][0], link[1][0]])
            c.append(link[1][0])
            break
        elif link[1][0] in c and link[0][0] not in c:
            c.append(link[0][0])
            break
    circuits.append([link[0][0], link[1][0]])

#now join circuits that share nodes
merged = True
while merged:
    merged = False
    new_circuits = []
    while circuits:
        c = circuits.pop(0)
        for oc in circuits:
            if set(c).intersection(set(oc)):
                c = list(set(c).union(set(oc)))
                circuits.remove(oc)
                merged = True
        new_circuits.append(c)
    circuits = new_circuits

lengths = np.array([len(x) for x in circuits])
lengths = np.sort(lengths)

print(f'Shortest lenghts: {lengths[-3:]} with product {np.prod(lengths[-3:])}')


#ok now for part two (making a greedy search)
# connections= [np.where(pairwise_distances == x) for x in b[1:n+1]]

circuits = []
index = 1
# for link in connections:

# Added link (array([342, 439]), array([439, 342])) to give 2 circuit with 1000 of 1000 nodes
# Added link (array([102, 712]), array([712, 102])) to give 1 circuit with 1000 of 1000 nodes


while sum([len(x) for x in circuits]) < len(data) or len(circuits) > 1:
    link = np.where(pairwise_distances == b[index])
    index += 1
    for c in circuits:
        if link[0][0] in c and link[1][0] not in c:
            # c.update([link[0][0], link[1][0]])
            c.append(link[1][0])
            break
        elif link[1][0] in c and link[0][0] not in c:
            c.append(link[0][0])
            break
    circuits.append([link[0][0], link[1][0]])

    #now join circuits that share nodes
    merged = True
    while merged:
        merged = False
        new_circuits = []
        while circuits:
            c = circuits.pop(0)
            for oc in circuits:
                if set(c).intersection(set(oc)):
                    c = list(set(c).union(set(oc)))
                    circuits.remove(oc)
                    merged = True
            new_circuits.append(c)
        circuits = new_circuits
    print(f'Added link {link} to give {len(circuits)} circuit with {sum([len(x) for x in circuits])} of {len(data)} nodes')

answer = np.prod(data[link[0]][:,0])

print(f'Part 2 answer: {answer}')