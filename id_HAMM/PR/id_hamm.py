#! /usr/bin/env python
"""
File Name: id_hamm.py
Created By: Peter Repiscak
Creation Date: [2019-04-07 16:32]
Last Modified: [2019-04-07 16:37]
Description: Calculate the Hamming distance between s and t strings specified on 1st and 2nd line.
"""
import argparse


def main():
    """
    python id_hamm.py --inputfile test.txt

    Calculate the Hamming distance between s and t strings specified on 1st and 2nd line.

    TO-DO:
    add check if file exists and connection is ok
    if there are two lines specified and their length is less than 1kbp
    and if they are DNA strings (ATCG)
    """
    arguments_parsed = argparse.ArgumentParser()
    arguments_parsed.add_argument(
        "-i",
        "--inputfile",
        required=True,
        help="input file needed")
    args = vars(arguments_parsed.parse_args())

    #print("Input file: {}".format(args["inputfile"]))

    with open(args["inputfile"], "r") as inputfile:
        data = inputfile.readlines()
        if len(data) == 2:
            print(sum([s != t for s, t in zip(data[0].rstrip(), data[1].rstrip())]))
        else:
            print(
                "file {} does not contain two strings on separate lines".format(
                    args["inputfile"]))


if __name__ == '__main__':
    main()
