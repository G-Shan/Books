formatter = "{} {} {} {}"

# It will print this " 1 2 3 4 " without double quotes
print(formatter.format(1, 2, 3, 4))
print(formatter.format("one", "two", "three", "four"))
print(formatter.format(True, False, False, True))

# This line would print 16 curly brackets
print(formatter.format(formatter, formatter, formatter, formatter))

print(formatter.format(
   "Try your",
   "owne text here",
   "or go",
   "screw yourself your dumb piece of garbage"

))
