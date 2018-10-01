

hairs = ["brown", "blond", "red"]
eyes = ["brown", "blue", "green"]
weights = [1, 2, 3, 4]

the_count = [1, 2, 3, 4]
fruits = ["apples", "oranges", "pears", "apricots"]
change = [1, "pennies", 2, "dimes", 3, "quarters"]

# This first kind of for-loop goes through a list
for number in the_count:
    print(f"This is count {number}")

# same as above
for fruit in fruits:
    print(f"A fruit of type: {fruit}")

# also we can go through mixed lists too.
# notice we have to use {} since we dont know whats in it
for i in change:
    print(f"I got {i}")

# We can also build lists, first start with an empty one
elements = []

# then use the range function to do 0 to 5 counts
for i in range(0, 6):
    print(f"Adding {i} to the list.")
    # append is a function that lists understand
    elements.append(i)


# Now we can print them out too
for i in elements:
    print(f"Elements was: {i}")
