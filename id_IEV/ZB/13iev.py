populations = input()
populations = populations.split(' ')

for i in range(len(populations)):
    populations[i] = int(populations[i])

    # AA-AA = 1
    # AA-Aa = 1
    # AA-aa = 1
    # Aa-Aa = 0.75
    # Aa-aa = 0.5
    # aa-aa = 0
probabilities = [1, 1, 1, 0.75, 0.5, 0]
Ex = 0
for i in range(len(probabilities)):
        Ex = Ex + 2*populations[i]*probabilities[i]
print(Ex)
