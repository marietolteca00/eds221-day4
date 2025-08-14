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


# Functions meet for loops!

# All data frames in the function are called df --> argument df
# Just use apply() bro! lol.
df_means <- function(df){
  for (i in 1:ncol(df)){ # i will go into the first column and last column of df
    if(is.numeric(df[[i]])){
    column_name <- colnames(df[i])
    col_mean <- mean(df[[i]], na.rm=TRUE)
    #print paste statment
    print(paste("The mean value of", column_name, "is", round(col_mean, 2)))
  }
}
}
# Try it
df_means(df = penguins)


## New section-
# Logistic growth example

# Logistic growth equation

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1+(( K - N0) / N0) * exp(-r * time))
  print(Nt)
}

# Check for one set up values
logistic_growth(N0= 100, K=6000, r=0.27, time=40)

# working on an example just dealing with time
time_vec <- seq(from = 0, to= 35, by= 0.1)

# apply the logistic growth function to that vector
pop_35 <- logistic_growth(N0=100, K=6000, r=0.27, time= time_vec)

# combining time steps and population size into a dataframe
pop_time_35 <- data.frame(time_vec, pop_35)

# Don't forget to load in tidyverse
library(tidyverse)
#ggplot
ggplot(data = pop_time_35, aes(x= time_vec, y= pop_35))+
  geom_line(size=0.5)

# Alternatively, with an internal for loop

# pre-allocate storage for output vector
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))

# for loop for stepping through time steps

for(i in seq_along(time_vec)){
population <- logistic_growth(N0=100, K=6000, r=0.27, time= time_vec[i])
pop_35_vec <- population
}

# now, building to estimating across growth rates

r_seq <- seq(from= 0.2, to=0.4, by=0.01)

# creating a MATRIX to store output values
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq)) 


for (j in seq_along(r_seq)){ # step through all of growth rates
for(i in seq_along(time_vec)){ # step through all of time vector
  # storing values into population- reinstall logistic growth equation
  population <- logistic_growth(N0=100, K=6000, 
                                r=r_seq[j], time= time_vec[i])
  out_matrix[i,j] <- population # adding [i, j] for 2-dimension. Store the value in the appropriate row and column
}
}


# Data wrangling to plot

# Adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec) #time column now added on dataframe

# update column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq),"time") # creating time vector on out_df in environment

# Pivot Longer to make it tidy
# pivot longer allows for all data to be stored under designated columns to view data
out_df_long <- out_df %>% 
  pivot_longer(cols = -time, 
               names_to = "growth_rate", 
               values_to = "population")

# PLot it

ggplot(data = out_df_long, aes(x=time, y=population)) +
  geom_line(aes(color = growth_rate)) +
  theme_minimal()

