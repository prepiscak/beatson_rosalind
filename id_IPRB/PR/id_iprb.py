#! /usr/bin/env python
"""
File Name: id_iprb.py
Created By: Peter Repiscak

Description: Mendel's First Law.

Given: Three positive integers k, m, and n, representing a population containing k+m+n organisms:
  k individuals are homozygous dominant for a factor, m are heterozygous, and nare homozygous recessive.

Return: The probability that two randomly selected mating organisms will produce an individual possessing a dominant allele
  (and thus displaying the dominant phenotype). Assume that any two organisms can mate.
"""
import argparse


def extractDataFromFile():
    """
    Opens file and extract k, m, n integers
    # test.txt file contains 3 integers for k, m, n separated by space
    python id_iprb.py --inputfile test.txt

    """

    arguments_parsed = argparse.ArgumentParser()
    arguments_parsed.add_argument(
        "-i",
        "--inputfile",
        required=True,
        help="input file needed")
    args = vars(arguments_parsed.parse_args())

    with open(args["inputfile"], "r") as inputfile:
        line = inputfile.readline().split()
        k, m, n = [int(x) for x in line]
    return(k, m, n)


def calcDomProb(k, m, n):
    """
    python id_hamm.py --inputfile test.txt

    Calculate the Hamming distance between s and t strings specified on 1st and 2nd line.

    TO-DO:
    add check if file exists and connection is ok
    if there are two lines specified and their length is less than 1kbp
    and if they are DNA strings (ATCG)

    # k+k = 100%
    # m+m = 75% (3/4) dominant
    # n+n = 0% dominant
    # k+m = 100%
    # k+n = 100%
    # m+n = 50% dominant
    """
    population1 = (k + m + n)
    population2 = (population1 - 1)
    P_kk = (k/population1)*((k-1)/population2)
    P_mm = (m/population1)*((m-1)/population2)
    P_nn = (n/population1)*((n-1)/population2)
    P_km = (k/population1)*(m/population2)  # 2x as P_km = P_mk
    P_kn = (k/population1)*(n/population2)  # 2x as P_kn = P_nk
    P_mn = (m/population1)*(n/population2)  # 2x as P_mn = P_nm

    Pr_total = P_kk + (3/4)*P_mm + 0*P_nn + 2*P_km + 2*P_kn + (1/2)*2*P_mn
    return(Pr_total)


if __name__ == '__main__':
    k, m, n = extractDataFromFile()
    print(calcDomProb(k, m, n))
