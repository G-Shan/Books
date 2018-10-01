
# Animal is-a object
class Animal(object):
    pass

# Dog is-a animal
class Dog(Animal):

    def __init__(self, name):
        # Dog has-a name
        self.name = name

#Cat is-a animal
class Cat(Animal):

    def __init__(self, name):
           ## Cat has-a name
            self.name = name

# Person is-a object
class Person(object):

    def __init__(self, name):
       ## Person has-a name
        self.name = name
        # Person has-a pet of some kind
        self.pet = None

# Employee is-a person
class Employee(Person):

    def __init__(self, name, salary):
        ##  run the __init__ method of a parent class reliably
        super(Employee, self).__init__(name)
        # Employee has-a salary
        self.salary = salary

# Fish is-a object
class Fish(object):
    pass

# Halibut is-a fish
class Halibut(Fish):
    pass

# rover is-a dog
rover = Dog("Rover")

#satan is-a cat
satan = Cat("Satan")

# mary is-a person
mary = person("Mary")

# From mary, get the pet attribute and set it to satanself.
# Or mary's pet is-a satan
mary.pet = satan

# frank is-a employee. his salary is 120000
frank = Employee("Frank", 12000)

#From frank, get pet and set it to rover
# frank's pet is rover
frank.pet = rover

# flipper is a fish
flipper = Fish()

# Set crouse is-a Salmon
crouse = Salmon()

# harry is Halibut.
harry = Halibut()
