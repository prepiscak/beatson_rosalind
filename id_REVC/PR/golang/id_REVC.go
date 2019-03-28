package main

import (  
    "fmt"
	"io/ioutil"
	"strings"
)

func complementString(r rune) rune {
	switch {
	case r == 'A':
		return 'T'
	case r == 'T':
		return 'A'
	case r == 'C':
		return 'G'
	case r == 'G':
		return 'C'		
	}
	return r
}

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
	  runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func main() {
    data, errInput := ioutil.ReadFile("rosalind_revc.txt") // just pass the file name
    if errInput != nil {
		fmt.Println("File reading error", errInput)
		return
    }

	/*
	func reverseBytes(s string) string {
		r := make([]byte, len(s))
		for i := 0; i < len(s); i++ {
			r[i] = s[len(s)-1-i]
		}
		return string(r)
	}
	*/

	// fmt.Println(data) // print the content as 'bytes'

    inputString := strings.TrimSpace(string(data)) // convert content to a 'string'

	fmt.Printf("Input string:\n%s\n", inputString) // print the content as a 'string'

	dataComplement := strings.Map(complementString, inputString)
	complementReversed := reverseString(dataComplement)

	fmt.Printf("Output string:\n%s\n", complementReversed) 

	// save to file
	// construct a byte array
	complementReversedByteArray := []byte(complementReversed)
	

	errOutput := ioutil.WriteFile("rosalind_revc_output.txt", complementReversedByteArray, 0777)
	// handle this error
	if errOutput != nil {
  	// print it out
  	fmt.Println("File writting error", errOutput)
	}
}