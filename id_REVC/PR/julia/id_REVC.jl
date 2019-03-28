filename_in = "rosalind_revc.txt"
filename_out = "rosalind_revc_output.txt"

#=
input_string = open(filename_in) do file
    read(file, String)
end
=#

#=
https://stackoverflow.com/questions/35782439/how-to-replace-several-characters-in-a-string-using-julia
https://stackoverflow.com/questions/41106546/julia-trim-string-whitespace-and-check-length
https://en.wikibooks.org/wiki/Introducing_Julia/Strings_and_characters

=#

input_string = strip(read(filename_in, String))

println("Input string:\n", input_string)

transtab = Dict('A' => 'T', 'C' => 'G', 'G' => 'C', 'T' => 'A')

output_string = reverse(join([transtab[c] for c in input_string]))

println("Output string:\n", output_string)
# writing to file
open(filename_out, "w") do file
    write(file, output_string)
end
