
ten_things = "Apples Oranges Crows Telephone Light Sugar"

print("Wait there are not 10 things in that list. Let's fix that.")

# This will split the object of ten_things. Spilliting would be done on the basis of space and a comma would be added to make the split
stuff = ten_things.split(' ')

more_stuff = ["Day", "Night", "Song", "Frisbee", "Corn", "Banana", "Girl", "Boy"]

# Here inside the loop, next_one will have last element of more_stuff and this element would be removed from more_stuff. Now this will keep go on
# until stuff length would be equal to 10
while len(stuff) != 10:
    next_one = more_stuff.pop()
    print("Adding:", next_one)
    stuff.append(next_one)
    print(f"There are {len(stuff)} items now.")

print("There we go:", stuff)

print("let's do some things and stuff.")

print(stuff[1])
print(stuff[-1])
print(stuff.pop())
print(' '.join(stuff)) # All elements of stuff would be joined back it with a space between each element
print('#'.join(stuff[3:5])) #Here we join the 3rd and 4th element with a hash. it will be Telephone#Light

# Page 169
