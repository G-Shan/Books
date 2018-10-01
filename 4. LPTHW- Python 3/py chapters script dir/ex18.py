

#This one is like your scripts with argv
def print_two(*args):
    arg1, arg2 = args
    print(f"arg1: {arg1}, arg2: {arg2}")

# What does the * in *args do? That tells Python to take all the arguments to the function and then put
# them in args as a list. It’s like argv that you’ve been using but for functions. It’s not normally
# used too often unless specifically needed.



#ok, that *args is actually pointless, we can just do This
# # Here 1st arg1 is being treated as string. 2nd arg1 inside bracket is being refrenced and formatted
def print_two_again(arg1, arg2):
    print(f"arg1: {arg1}, arg2: {arg2}")

#this just takes one argument
def print_one(arg1):
    print(f"arg1: {arg1}")

#this one takes no arguments
def print_none():
    print("I got nothin'. ")

print_two("Sohail", "Ahmad")
print_two_again("Zed", "Shaw")
print_one("First!") # #It will print "arg1: Shut up, arg2: Biatch"
print_none()
