
from sys import argv
from os.path import exists # exsits tell me wethera file exists or not based on its name in a string as an argument

# Script would be the name of this file. We would have to mention from_file and to_file
script, from_file, to_file = argv


print(f"Copying from {from_file} to {to_file}")

in_file = open(from_file) #Opening the from_file and saving it as in_file
indata = in_file.read() # reading the in_file and saving it into indata

print(f"The input file is {len(indata)} bytes long")

print(f"Does the output file exist? {exists(to_file)}") # DOes this file exist? Yes/No
print("Read, hit RETRUN to continue, CTRL-C to abort.")
input() #It will ask for input

out_file = open(to_file, 'w') # out_file would be in the write mode
out_file.write(indata) # We write info in Out_file from indata

print("Alright, all done.")

out_file.close() # CLosing the out_file
in_file.close()
