
from sys import argv

# script is the name of this python file i.e. ex14.py. We would have to give inputs for first, second and third parameters

script, first, second, third = argv

print("THis script is called:", script)
print("your first variable is:", first)
print("Your second variable is:", second)
print("Your third variable is:", third)

# To run these variables in from powershell, type python ex13.py first 2nd 3rd
# If we want diff arguments, then python ex13.py orange, apple, kela
