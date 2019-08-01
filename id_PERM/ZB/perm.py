def permutation(numList):
    permAll = []
    if len(numList) == 0:
        return []

    if len(numList) == 1:
        return [numList]

    for i in range(len(numList)):

        first = numList[i]
        rest = numList[:i] + numList[i+1:]
        # print(rest)
        restList = permutation(rest)
        # print(restList)
        for rests in restList:
            permAll.append([first] + rests)
            # print(permAll)

    return permAll
#############################################################################

number = int(input())

numList = [str(x) for x in range(1,number + 1,1)]
print(numList)
permutations = permutation(numList)
# print(permutations)
print('========results===============')
print(len(permutations))
for perm in permutations:
    # ''.join(perm)
    # [int(i) for i in perm]
    print(' '.join(perm))
