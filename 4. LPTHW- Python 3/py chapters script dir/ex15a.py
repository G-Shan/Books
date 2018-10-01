
from sys import argv

# This line would ask for input from script and filename from user. Type "python ex15a.py ex15_sample.txt" then this script would run
script, filename = argv

#Here we open the file and save it in an object
txt = open(filename)

# It will print Here's your file XXX
print(f"Here's your file {filename}:")

# This line would print the text that is inside the file XXX
print(txt.read())

# THis line would print "Type the filename again"
print("Type the filename again:")
file_again = input("> ") # This line would print prompt the user to give the input. i.e. filename

# THis owuld create an object txt_again
txt_again = open(file_again)
print(txt_again.read())
