#!/usr/bin/env python

def prob_mass_func(n,k,prob):
    """
    Probability mass function:
        The probability of getting exactly k successes in n trials.
        https://en.wikipedia.org/wiki/Binomial_distribution
    # [] hardcode factorial?! (but math.factorial is faster written in C!)
    """
    from math import factorial as fact
    # prob = prob_AaBb
    return((fact(n)/(fact(k)*fact(n-k)))*(prob**k)*(1-prob)**(n-k))

def pseudo_cdf(generations, successes, offsprings_per_generation=2, prob_offspring=0.25):
    """
    Calculates "pseudo" cummulative distribution function as k is not a "floor", see
    https://en.wikipedia.org/wiki/Binomial_distribution
    
    Hardcoded variables:
    prob_offspring = prob_AaBb = 0.25
    offsprings_per_generation = 2
    """
    
    offsprings = offsprings_per_generation**generations
    prob_result = 0

    # iterating over all successes equal and greater than k (at least)
    while successes <= offsprings:
        prob_result = prob_result + prob_mass_func(offsprings,successes,prob_offspring)
        successes += 1
        
    return(prob_result)

if __name__ == '__main__':
    """
    TODO:
    [] load file and extract k, N
    """
    #import sys
    # [] read inputs from file
    # [] assert restrictions on k (k<=7), N (N<=2**k)
    #k, N = sys.argv[1], sys.argv[2]
    generations = int(input("Enter number of generations (k): "))
    successes = int(input("Enter minimum number of AaBb in k-th generation: "))

    total_prob_success = pseudo_cdf(generations, successes)
    print("The probability at least {} AaBb organisms will belong to the {}-th generation is:\n {:.3f}".format(successes, generations, total_prob_success))