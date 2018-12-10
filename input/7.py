from collections import defaultdict, deque

E = defaultdict(list)
D = defaultdict(int)
for line in open('day7.txt'):
	words = line.split()
	x = words[1]
	y = words[7]
	E[x].append(y)
	D[y] += 1
        
        print(x)
