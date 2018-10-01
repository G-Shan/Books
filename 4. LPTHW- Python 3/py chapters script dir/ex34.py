
animals = ['bear', 'python3.6', 'peacock', 'kangaroo', 'whale', 'platypus']
# For each of these, write out a full sentence of the form: ”The first (1st) animal is at 0 and is a bear.” Then
# say it backwards: ”The animal at 0 is the 1st animal and is a bear.”


# Solution
okay = ['1st', '2nd', '3rd', '4th', '5th', '6th']

def anime(animals, okay, i):
    print(f"The " + okay[i]+ " animal is at "+ str(i)+ " and is:", animals[i])

for i in range(len(okay)):
    anime(animals, okay, i)
