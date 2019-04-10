#!/usr/bin/env python3

'''
Mendel's First Law

The first law concludes that for any factor, an organism randomly passes one of its two alleles to each offspring, so that an individual receives one allele from each parent.
Mendel also believed that any factor corresponds to only two possible alleles, the dominant and recessive alleles. An organism only needs to possess one copy of the dominant allele to display the trait represented by the dominant allele. In other words, the only way that an organism can display a trait encoded by a recessive allele is if the individual is homozygous recessive for that factor.
We may encode the dominant allele of a factor by a capital letter (e.g., A) and the recessive allele by a lower case letter (e.g., a). Because a heterozygous organism can possess a recessive allele without displaying the recessive form of the trait, we henceforth define an organism's genotype to be its precise genetic makeup and its phenotype as the physical manifestation of its underlying traits.

Given: Three positive integers k, m, and n, representing a population containing k+m+n organisms: 
k individuals are homozygous dominant for a factor (both have same allele), 
m are heterozygous (have different alleles), 
and n are homozygous recessive.
Return: The probability that two randomly selected mating organisms will produce an individual possessing a dominant allele (and thus displaying the dominant phenotype). Assume that any two organisms can mate.
'''
'''
If P(d) is the probability that the offspring has a dominant allele (d), then there are 5 potential outcomes each producing it's own punnet square:
1. Both parents homozygous dominant - dd
2. Homozygous dominant and hetrozygous - dh
3. Both hetrozygous - hh
4. Hetrozygous and homozygous recessive - rh
5. Both homozygous recessive - rr

There are fewer recessive outcomes so calculate this.
rr = (n/t)*((n-1)/(t-1)) - for 6
rh = (n/t)*(m/(t-1)) + (m/t)*(n/(t-1)) - for 5
hh = (m/t)*((m-1)/(t-1))

Punnet square values for ressive conditions
rr -> 4/4 = 1
rh -> 2/4 = 0.5
hh -> 1/4 = 0.25

Total probability for recessive condition
res = (rr * 1) + (rh * 0.75) + (hh * 0.25)
And the probability of getting dominiant is:
dom = 1 - res
'''

def mendelProb(k, m, n):
    t = k + m + n
    # calculate res than subtract 1
    res = ((n/t)*((n-1)/(t-1)) * 1) + (((n/t)*(m/(t-1)) + (m/t)*(n/(t-1))) * 0.5) + ((m/t)*((m-1)/(t-1)) * 0.25)
    dom = 1 -res
    return dom


def test():
    print("{:.5f}".format(mendelProb(2, 2, 2)))

#test()
# 23 26 21
print("{:.5f}".format(mendelProb(23, 26, 21)))