
# Creating a mapping of state to abbreviation
states = {
  'Oregan': 'OR',
  'Florida': 'FL',
  'California': 'CA',
  'New York': 'NY',
  'Michigan': 'MI'
}

# Create a set of states and some cities in them
cities = {
  'CA': 'San Fransisco',
  'MI': 'Detroit',
  'FL': 'JacksonVille'
}

# Add some more cities
cities['NY'] = 'New NewYork'
cities['OR'] = 'Portland'

# print out some cities
print('_' * 10)
print("Ny State has:", cities['NY'])
print("OR State has:", cities['OR'])


# Print some states
print('_' * 10)
print("Michigan's abbreviation is:", states['Michigan'])
print("Florida has:", cities[states['Florida']])


# We can access keys & elements of a dict using states.items(). using list(states.items()), we can get them as a list
# We can also get the keys of the dict using states.keys()


# Here we run a for loop where state is for key and abbrev is for element. Here we use two items (state, abbrev) because
# list(states.items()) would print the values as a pair (key & its element).
# Print every state abbreviation
print('_' * 10)
for state, abbrev in list(states.items()):
    print(f"{state} has the abbreviated {abbrev}!!!")

print("\n")

# Print every city in states
print('-_' * 10)
for kek, city in list(cities.items()):
    print(f"{kek} has the city {city}..")

print("\n")

# Now do both at the same time
print('_!' * 10)
for state, abbrev in list(states.items()):
    print(f"{state} stata is abbreviated {abbrev}!1!")
    print(f"and has city {cities[abbrev]}..")


print("\n")

#get() method returns the value for the given key, if present in the dictionary. If not, then it will return None
print('_._' * 10)
#safely get an abbreviation by state thta might not be There
state = states.get('Texas')
if not state:
    print("Sorry, no Texas.")


# Morex example of get() function
print(states.get("Florida")) # It will print the element for florida key
print(states.get("Bihar", "Not found honey!")) # Print the elelment for Bihar key. If its not there, print "not found honey"

# Get a city with a default value
city = cities.get('TX', "does not exist.") #if TX is present then return its value. if not then return "does not exist"
print(f"The city for the state 'TX' is: {city}")
