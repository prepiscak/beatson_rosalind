#!/usr/bin/env python3

'''
Counting Point Mutations

Problem

Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of corresponding symbols that differ in s and t.

Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
Return: The Hamming distance dH(s,t).
'''
import os


def hamm(s, t):
    dH = 0
    for l in range(0, len(s) - 1):
        if s[l] != t[l]:
            dH += 1
    return dH

def test():
    test_s = "GAGCCTACTAACGGGAT"
    test_t = "CATCGTAATGACGGCCT"
    print("Hamming Distance:")
    print(hamm(test_s, test_t))

def main():
    fn = input("Please enter path, 'test' for test data or 'q' to quit: ")
    if fn == 'test':
        test()
    elif fn == 'q':
        exit()
    else:
        # Check file exists and if so load in data
        exists = os.path.isfile(fn)
        if exists:
            # Store configuration file values
            print("Found")
            # Load data and split into two
            with open(fn, 'r') as fhand:
                dt = fhand.read().split('\n')
                print("Hamming Distance:")
                print(hamm(dt[0], dt[1]))

        else:
            print("Sorry, couldn't find the file.")
            print()
            main()


main()