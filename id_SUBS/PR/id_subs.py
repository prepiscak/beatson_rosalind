#! /usr/bin/env python
"""
File Name: id_subs.py
Created By: Peter Repiscak
Description: Returns all positions of motif_string (t) in input_string (s).
"""
import argparse


def motif_position(input_string, motif_string):
    """
    Given input_string and motif_string

    Returns all positions of motif_string in input_string
    #!!! returns 1 based start

    """
    
    motif_match_positions = [str(position+1) for position, char in enumerate(input_string) if char == motif_string[0] and input_string[position:position+len(motif_string)] == motif_string]

    return(motif_match_positions)


def main():
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
            input_string = data[0].rstrip()  #s
            motif_string = data[1].rstrip()  #t
            
            print(" ".join(motif_position(input_string, motif_string)))
            
        else:
            print(
                "file {} does not contain two strings (input_string and motif_string) on separate lines".format(
                    args["inputfile"]))


if __name__ == '__main__':
    main()
