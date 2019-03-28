/* https://www.freecodecamp.org/forum/t/how-to-read-local-text-file-into-a-js/231112
https://www.w3schools.com/nodejs/nodejs_filesystem.asp
https://stackoverflow.com/questions/10058814/get-data-from-fs-readfile
 */
const fs = require('fs');
var string = fs.readFileSync('rosalind_revc.txt','utf8');

// remove empty space from print!!!
console.log('Input string:\n', string);

const mapping = {'A':'T', 'T':'A', 'C':'G', 'G':'C'};

// use . wildcard or just to be sure specify known nucleotides; e.g. .replace(/./g,...)
// string.replace(searchvalue, newvalue)
// https://www.w3schools.com/jsref/jsref_replace.asp
// https://www.w3schools.com/js/js_regexp.asp
var reversed_string = string.split("").reverse().join("");
var reversed_complement = reversed_string.replace(/A|T|C|G/g, function(x) {return mapping[x]}); // g - global; gi - global case-sensitive search

/* or using arrow functions: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions
https://codeburst.io/javascript-arrow-functions-for-beginners-926947fc0cdc
*/
// var result = string.replace(/A|T|C|G/g, (character) => mapping[character]); 

/* reverse string: 
https://medium.com/quick-code/5-ways-to-reverse-a-string-in-javascript-466f62845827
https://medium.freecodecamp.org/how-to-reverse-a-string-in-javascript-in-3-different-ways-75e4763c68cb
https://medium.com/sonyamoisset/reverse-a-string-in-javascript-a18027b8e91c
*/


console.log('Reversed complement:\n', reversed_complement);
// console.log(mapping['A'])
// console.log(string)
/* 
var mapping = {'A':'T', 'T':'A', 'C':'G', 'G':'C'}
var string = "AAATTT"
var result = string.replace(/./g, (character) => mapping[character])

console.log(result)
 */
