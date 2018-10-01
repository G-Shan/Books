from sys import argv

# script is the name of this python file i.e. ex14.py. We would have to give inpyt for user_name
script, user_name = argv
prompt = '>'

print(f" Hi {user_name}, I'm {script} script.")
print("I'd like to ask you a few questions.")
print(f"Do you like me {user_name}?")
likes = input(prompt) # It will print ">" and ask for the user input

print(f"Where do you live {user_name}?")
lives = input(prompt)

print("What kind of computer do you have?")
computer = input(prompt)

print(f"""
Alright mate, so you said {likes} about liking meself.
You live in {lives}. Not sure where that is.
And you have a {computer} computer. Nice.
"""
)
