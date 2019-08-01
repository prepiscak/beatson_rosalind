
def main():
    """
    python id_PERM.py --inputfile test.txt
    
    Calculate all possible permutations of length n of the positive integers [1,2,…,n]. 
    Where n should be provided on a command line or in the .txt file.

    TODO:
    finish parsing arguments! and handle empty etc.
    """
    import argparse
    import itertools

    arg_parser = argparse.ArgumentParser()
    args_group = arg_parser.add_mutually_exclusive_group(required=True)
    args_group.add_argument("-i", "--inputfile", help="input file with length of permution list of positive integers")
    args_group.add_argument("-n", "--inputnumber", help="input length of permution list of positive integers", type=int)
    args = vars(arg_parser.parse_args())
    
    input_file = args["inputfile"]
    if input_file:
        with open(input_file, 'r') as file:
            perm_length = int(file.readline())
    else:
        perm_length = args["inputnumber"]

    perm_list = [i for i in range(1, perm_length+1)]
    all_perm = list(itertools.permutations(perm_list))

    print(len(all_perm))
    for permutation in all_perm:
        print(*permutation)  

if __name__ == '__main__':
    main()