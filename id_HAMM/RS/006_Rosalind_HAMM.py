#!/usr/bin/env python3

'''
Counting Point Mutations

Problem

Given two strings s and t of equal length, the Hamming distance between s and t, denoted dH(s,t), is the number of corresponding symbols that differ in s and t.

Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
Return: The Hamming distance dH(s,t).
'''
import os
import re


def allowed_match(strg, search=re.compile(r'[^GCTA.]').search):
    return not bool(search(strg))


def hamm(s, t):
    dH = 0
    for l in range(0, len(s)):
        if s[l] != t[l]:
            dH += 1
    return dH


def test():
    test_s = "GAGCCTACTAACGGGATA"
    test_t = "CATCGTAATGACGGCCTG"
    print(test_s)
    print(test_t)
    print("Hamming Distance:")
    print(hamm(test_s, test_t))


def inputSequence():
    s_i = input("Input first sequence ")
    t_i = input("Input second sequence: ")
    if s_i == 'q' or t_i == 'q':
        print("Quiting")
        exit()
    elif len(s_i) != len(t_i):
        print("Sequences of different length. Please re-enter")
        inputSequence()
    elif allowed_match(s_i) or allowed_match(t_i):
        print("Illegal characters in sequence. Please re-enter")
        inputSequence()
    else:
       print(hamm(s_i, t_i))  


def check_file(lst):
    if len(lst[0]) != len(lst[1]):
        return False
    elif allowed_match(lst[0]) or allowed_match(lst[1]):
        return False
    else:
        return True


def main():
    fn = input("Please enter path, 'test' for test data, 'input' to input sequences or 'q' to quit\n")
    if fn == 'test':
        test()
    elif fn == 'input':
        inputSequence()
    elif fn == 'q':
        exit()
    else:
        # Check file exists and if so load in data
        exists = os.path.isfile(fn)
        if exists:
            # Store configuration file values
            print("File found")
            # Load data and split into two
            with open(fn, 'r') as fhand:
                dt = fhand.read().split('\n')
                    #check_file(dt)
                if True:
                    print("Hamming Distance:")
                    print(hamm(dt[0], dt[1]))
                else:
                    print("Sorry, problem with sequences in file")
                    main()
        else:
            print("Sorry, couldn't find the file.")
            print()
            main()


main()