# adds up the number of birds and dogs

# Defined function
birddog_sum <- function(bird, dog){ #place holder variables - can also put x ,y
  pets <- bird + dog # Argument
  return(pets) # Will store it so it can be used later
} # Return will show up as a preview, will not store in Environment Pane!

# Use it
# Assinging it to a variable for it to be stored in Environment Pane !
total_pets <- birddog_sum(bird = 2, dog = 5)
# Same thing as above
total_pets <- birddog_sum(2, 5)

# create a function to double values
double_it <- function(x){ # x is the only one that will change
  print(2 * x)
}

double_it(24) # this is x

# Write a function with conditionals
# Example is converting animals ages

animal_age <- function(animal, age){
  if (animal  == "dog"){
    print(age * 7) # age is our place holder variable
    } else if (animal == "goat"){
      print(age * 4.7)
      }
}

# Try using for an 8 year old dog
animal_age(animal = "dog", age = 8) #NOT WORKING

# try using for a cow
animal_age(animal = "cow", age= 8) #have not assign "cow" in the for loop

# Write an updated function of the animal age function with error messages

animal_age_stop <- function(animal, age){
  # ! - in operator: "!animal" is not in the vector
  
  if (!animal %in% c("dog", "goat")){
    stop(" Oops! Animal must be a dog or goat.")
  }
  
  if(is.numeric(age) == FALSE) {
    stop(" The age must be a number without QUOTES.")
  }
  
  if(age <= 0 | age > 100){
    stop(" Are you sure about your animal's age?")
  }
  
  if (animal  == "dog"){
    print(age * 7) # age is our place holder variable
  } else if (animal == "goat"){
    print(age * 4.7)
  }
}

# Calling animal_age_stop
animal_age_stop("elephant", 10) # elephant not included in for loop

animal_age_stop("dog", 100) # 700 old dog

