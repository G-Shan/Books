# Here we can access a list element through index. We can also replace the element of a list with its index
# Howeever unlike dictionaries, we can only use numbers to get elements from a list
things = ['a', 'b', 'c', 'd']
print(things[1])

things[1] = 'z' # Here we replace b with z
print(things[1]) # The list would have elements as ['a', 'z', 'c', 'd']
things


stuff2 = {'Name': 'Sohail', 'Age' : 39, 'Height': 6*12+2}
print(stuff2['name'])

# We can add new keys in the dictionary. This will add 'city': 'SF'
stuff2['city'] = "SF"
print(stuff2['city'])

# It will add another key where 1:Sweet
stuff2[1] = "Sweet"
stuff2

# We can also delete elements
del stuff2[1]
print(stuff2)
del stuff2['city']
print(stuff2)
