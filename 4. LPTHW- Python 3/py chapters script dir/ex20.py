
# Type "ex20.py ex20_test.txt" to run this file. THere is a text file ex20_test in the directory
from sys import argv

script, input_file = argv

# This function would read lines from a file
def print_all(f):
    print(f.read())

# This would rewind the file and reset the file at zero byte in the file itself
def rewind(f):
    f.seek(0)

# This function would take two argumet, and give the line number with the actual line itself
def print_a_line(line_count, f):
    print(line_count, f.readline())

current_file = open(input_file)

print("First let's print the whole file: \n")

print_all(current_file)

print("Now let's rewind, kind of like a tape.")

rewind(current_file)

print("Let's print three lines.")

current_line = 1
print_a_line( current_line,current_file)

current_line = current_line + 1
print_a_line(current_line, current_file)
