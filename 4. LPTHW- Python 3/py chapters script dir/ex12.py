
# When you typed input() you were typing the ( and ) characters, which are parenthesis characters.
# For input you can also put in a prompt to show to a person so he knows what to type. Put a string that
# you want for the prompt inside the () so that it looks like this:
# y = input ( ”Name? ” )
# This prompts the user with ”Name?” and puts the result into the variable y. This is how you ask someone
# a question and get the answer.
# This means we can completely rewrite our previous exercise using just input to do all the prompting.




age = input("How old are you?")
height = input("How tall are you? ")
weight = input("How much do you weigh? ")

print(f"So, you're {age}, {height} tall and {weight} heavy. ")
