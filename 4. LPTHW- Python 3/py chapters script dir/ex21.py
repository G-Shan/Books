
# It will add two values and print "Adding 4 +5, and return 9"
def add(a, b):
  print(f"Adding {a} + {b}")
  return a + b

def subtract(a, b):
  print(f"subtracting {a} - {b}")
  return a - b

def multiply(a, b):
  print(f"multiplying {a} / {b}")
  return a * b

def divide(a, b):
  print(f"Dividing {a} / {b}")
  return a / b

print("Let's do some math with just functions")

age = add(30, 5)
height = subtract(78, 4)
weight = multiply(90, 2)
iq = divide(100, 2)

print(f" Age: {age}, height: {height}. Weight: {weight}, IQ: {iq} ")

print("Here;s the puzzle.")

what = add(age, subtract(height, multiply(weight, divide(iq, 2))))

print("The become:", what, "can you do it by hand?")
