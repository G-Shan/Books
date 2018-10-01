
from sys import argv

# Script would be the name of this file i.e. ex16.py. We would have to mention a name for filename
script, filename = argv

print(f"We're going to erase {filename}.")
print("If you dont want that, hit CTRL-C (^C).")
print("If you do want that, hit RETRUN.")


# It will ask for the input. Mention input as either RETURN or filename XX
input("?") # Suppose, we give XX as input. Then a file XX would be created

print("Opening the file....")
target = open(filename, "w") # W means write  mode

print("Truncating the file. Goodbye!")
target.truncate() #This file would be emptied

# You need to give here three inputs for three lines
print("Now I'm going to ask you for three lines.")
line1 = input("line 1:")
line2 = input("line 2:")
line3 = input("line 3:")

print("I'm going to write these to the file")
# The three lines would be written in the file XXX; one line in each row
target.write(line1)
target.write("\n")
target.write(line2)
target.write("\n")
target.write(line3)
target.write("\n")


print("And finally, we close it.")
target.close() #File would be closed after being written
