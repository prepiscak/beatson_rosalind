use std::fs;

fn main() {
    // --snip--
    // println!("Input file {}", filename);

// for .pop() to work change let input_string to let mut input_string (for muttability)
    let mut input_string = fs::read_to_string("rosalind_revc.txt").expect("Unable to read file");
    // potentially remove newline input_string_nonewline = input_string.trim().to_string();
    input_string.pop(); // pops the last character; or for other approaces see: 
    // https://stackoverflow.com/questions/37888042/remove-single-trailing-newline-from-string-without-cloning

    println!("Input string:\n{}", input_string);

// creating complement 
/* Resources: 
https://stackoverflow.com/questions/34606043/how-do-i-replace-specific-characters-idiomatically-in-rust
https://stackoverflow.com/questions/27996430/reversing-a-string-in-rust
https://lise-henry.github.io/articles/optimising_strings.html
for benchmarking: https://www.reddit.com/r/rust/comments/8tplwr/what_is_the_standard_way_to_run_benchmarks_in/
*/

    let complement_string:String = input_string.chars()
        .map(|x| match x { 
            'A' => 'T', 
            'T' => 'A', 
            'C' => 'G',
            'G' => 'C',
            _ => x}
            ).collect();

/*
    let complement_string = str::replace(&input_string, "A", "N");
*/
    println!("Output string:\n{}", complement_string.chars().rev().collect::<String>());

// add saving to file
}