#Day 5####
##Swader: Intro to R####

# Load Data####

library(readr)
flfp <- readRDS("flfp-individual-level.rds")

# Run simple model ####

library(broom)
simple_model <- lm(data=flfp, patr_values~ religious + age_gr +edu)
tidy(simple_model)

# Simple multiverse set up####
#Make a vector of denominations
denominations <- levels(flfp$denom)
print(denominations)
#Put age categories in correct order
flfp$age_gr <-   factor(flfp$age_gr,
                        ordered = TRUE,
                        levels = c("18-25", "26-35","36-45", "46-55", "56-65", ">66"))
age_category_cutoff <- levels(flfp$age_gr )

#because we are going to use this to define who belongs to the lower age group, that means that we don't need the upper one. 
#so we cut it off
age_category_cutoff <- age_category_cutoff[-length(age_category_cutoff)]
print(age_category_cutoff)


# First for loop####


#now we make a loop of the denominations
for (denom in denominations){  
  #denom is a new variable created by the loop
  #denom changes for each iteration of denominations
  #let's print and see if it works
  print(denom)
}

# Second for loop####
#now we make a loop of the age group cutoffs within the other loop
#ALERT: nested loops (loops within loops) are slow... but since i only have a few items in each list (and not thousands), it won't matter here. 
for (denom in denominations){  
  for (age_cutoff in age_category_cutoff){ #notice I indent here to keep track of the hierarchy of loops
    
    #I again print something out to make sure i get the desired result
    #cat is a wonderful way to put together your own print messages
    cat("\n", denom, "and",age_cutoff, "combination") 
    
  }
}

# Let's instead loop by an index number####
#adapting code for my best practice 
for (denom in 1:length(denominations)){  
  for (age_cutoff in 1:length(age_category_cutoff)){
    cat("\n", denominations[denom], "and",age_category_cutoff[ age_cutoff], "combination") 
    
    #now we have unique combinations of religious denomination and the age category cutoff to work with.
    
  }
}

# Choose the religious denomination subsample####
#adapting code for my best practice 
for (denom in 1:length(denominations)){  
  for (age_cutoff in 1:length(age_category_cutoff)){
    
    this_denomination <- denominations[denom]
    temporary_flfp <- flfp[flfp$denom==this_denomination & !is.na(flfp$denom) ,]

     print(nrow(temporary_flfp)) #the number of rows should differ if the filtering worked!
    #notice how we again use print() or cat() to print out the output to make sure it looks correct!
    
  }
}

# Your turn ####
library(tidyverse)
for (denom in 1:length(denominations)){  
  for (age_cutoff in 1:length(age_category_cutoff)){
    
    this_denomination <- denominations[denom]
    temporary_flfp <- filter(flfp, denom==this_denomination)
    print(nrow(temporary_flfp)) #the number of rows should differ if the filtering worked!
    #notice how we again use print() or cat() to print out the output to make sure it looks correct!
    
  }
}

# Dichotomize the age_group variable####
library(dplyr)

for (denom in 1:length(denominations)){  
  for (age_cutoff in 1:length(age_category_cutoff)){
    
    this_denomination <- denominations[denom]
    temporary_flfp <- flfp[flfp$denom==this_denomination,]
    this_age_cutoff <- age_category_cutoff[age_cutoff]
    age_gr_lower_group <- age_category_cutoff[1:age_cutoff]
    temporary_flfp$age_gr <-  ifelse(temporary_flfp$age_gr %in% age_gr_lower_group,"younger","older") 
    temporary_flfp$age_gr <- as.factor(temporary_flfp$age_gr)
    print(sum( temporary_flfp$age_gr=="younger"))
    
    
  }
}

# Run the regression and save the results ####

results_list <- vector("list", length =0)

for (denom in 1:length(denominations)){  
  for (age_cutoff in 1:length(age_category_cutoff)){
    
    this_denomination <- denominations[denom]
    temporary_flfp <- flfp[flfp$denom==this_denomination,]
    this_age_cutoff <- age_category_cutoff[age_cutoff]
    age_gr_lower_group <- age_category_cutoff[1:age_cutoff]
    temporary_flfp$age_gr <-  ifelse(temporary_flfp$age_gr %in% age_gr_lower_group,"younger","older") 
    temporary_flfp$age_gr <- as.factor(temporary_flfp$age_gr)
    
    #here is a way to enter in a regression formula so that the dv and ivs are changing, in case you would want different dvs and ivs to enter your multiverse analysis. here they are stable, but we merely use a differently defined age_gr variable and a different sample.
    
    dv <- "patr_values"
    ivs <- c("religious", "age_gr", "edu")
    
    f <- as.formula(
      paste(dv,
            paste(ivs, collapse = " + "),
            sep = " ~ "))
    
    this_regression <- eval(bquote(   lm(.(f), data = temporary_flfp)   ))
    
    
    this_model_iteration <- t(data.frame(this_regression$coefficients))
    
    
    results_list <- append(results_list, list(this_model_iteration))
    
    
  }
}

results_list #look at the results. Call out an individual list member with results_list[[1]]

# While loop####
threshold_to_beat <- mean(bind_rows( lapply(results_list, data.frame))[,1]) # we take the largest intercept of the 20 models run

this_intercept <- 0 #we start the test intercept number at zero
intercepts <- vector("numeric",0)
while(this_intercept <=threshold_to_beat){
  
  this_sample <- sample(1:nrow(flfp), 5000, replace = F) #indices of this new subset
  
  temporary_flfp <- flfp[this_sample,]
  
  simple_model <- lm(data=temporary_flfp, patr_values~ religious + age_gr +edu)
  this_intercept <-  tidy(simple_model)[1,2]
  intercepts <- c(intercepts, this_intercept)
  #print(unlist(unname(this_intercept)))
  
}

summary(temporary_flfp) #we can print a summary of this sample's characteristics to compare it with the the groups we have tested

# If statements####
#we add an if statement and a counter inside the above while loop. We want to add a count so that every 100th iteration we get a message.

threshold_to_beat <- mean(bind_rows( lapply(results_list, data.frame))[,1]) 

this_intercept <- 0 
intercepts <- vector("numeric",0)
counter <- 0 #we use this counter to count iterations within the while loop

while(this_intercept <=threshold_to_beat){
  counter <- counter+1#here the counter ticks forward
  
  this_sample <- sample(1:nrow(flfp), 5000, replace = F) 
  
  temporary_flfp <- flfp[this_sample,]
  
  simple_model <- lm(data=temporary_flfp, patr_values~ religious + age_gr +edu)
  this_intercept <-  tidy(simple_model)[1,2]
  intercepts <- c(intercepts, this_intercept)
  
  if ((counter %% 10) ==0){ # %% calculates the REMAINDER of division. So the remainder of x  divided by 10 is zero if x is some multiple of 10. In other words, this if statement will trigger every 10th iteration
    cat("\nIteration number is", counter) #A message will be trigger by this statement
  }
  
} #end while loop

#summary(temporary_flfp) 

# Functions####
#this is how we define a function from our while loop above
random_subset <- function(threshold_to_beat=0){
  #we define the arguments that the function will take
  #we can set a default if we wish by setting equals to our desired value. 
  
  #we comment this out, because the user will enter this in as an argument!
  #  threshold_to_beat <- mean(bind_rows( lapply(results_list, data.frame))[,1]) 
  
  this_intercept <- 0 
  intercepts <- vector("numeric",0)
  counter <- 0 #we use this counter to count iterations within the while loop
  
  while(this_intercept <=threshold_to_beat){
    counter <- counter+1#here the counter ticks forward
    
    this_sample <- sample(1:nrow(flfp), 5000, replace = F) 
    
    temporary_flfp <- flfp[this_sample,]
    
    simple_model <- lm(data=temporary_flfp, patr_values~ religious + age_gr +edu)
    this_intercept <-  tidy(simple_model)[1,2]
    intercepts <- c(intercepts, this_intercept)
    
    if ((counter %% 10) ==0){ # %% calculates the REMAINDER of division. So the remainder of x  divided by 10 is zero if x is some multiple of 10. In other words, this if statement will trigger every 10th iteration
      cat("\nIteration number is", counter) #A message will be trigger by this statement
    }
    
  } #end while loop
  
  
  #functions should usually output something. we specify this using return() 
  return(temporary_flfp)
  
  
}

# Running the new function####
#we run the function code above, so that the function is known to R and loaded in the memory (just like when we create any other new object)

#then we call the function like any other
use_this_threshold <- mean(bind_rows( lapply(results_list, data.frame))[,1]) 
my_results <- random_subset(threshold_to_beat = use_this_threshold)

#you can look in myresults as you wish. e.g. using View().
#you can also have your functions output a variety of different information, e.g. in the form of a list
#e.g. results(list(subset=temporary_flfp, iteration_number=counter))


# Debugging####
# I expand this function, adding some bugs as well

random_subset_new <- function(threshold_to_beat=0){
  #we define the arguments that the function will take
  #we can set a default if we wish by setting equals to our desired value. 
  
  #we comment this out, because the user will enter this in as an argument!
  #  threshold_to_beat <- mean(bind_rows( #lapply(results_list, data.frame))[,1]) 
  
  this_intercept <- 0 
  intercepts <- vector("numeric",0)
  counter <- 0 #we use this counter to count iterations within the while loop
  
  while(this_intercept <=threshold_to_beat){
    counter <- counter+1#here the counter ticks forward
    
    this_sample <- sample(44671, 5000, replace = T) 
    
    temporary_flfp <- flfp[this_sample,]
    
    simple_model <- lm(data=temporary_flfp, patr_values~ religious + age_gr +edu)
    this_intercept <-  tidy(simple_model)[1,2]
    intercepts <- c(intercepts, this_intercept)
    
    if ((counter %% 10) ==0){ # %% calculates the REMAINDER of division. So the remainder of x  divided by 10 is zero if x is some multiple of 10. In other words, this if statement will trigger every 10th iteration
      cat("\nIteration number is", counter) #A message will be trigger by this statement
    }
    
  } #end while loop
  
  
  
  return(summary(temporary_flfp), counter) 
  
  
}

# Running the bugged function

#we source the function code above, so that the function is known to R

#then we call it like any other function
my_results <- random_subset_new(threshold_to_beat =mean(bind_rows( lapply(results_list, data.frame))[,1])  )

#you can look in myresults as you wish.
#you can also have your functions output a variety of different information, e.g. in the form of a list
#e.g. results(list(summary=summary(temporary_flfp), iteration_number=counter))

# Help!####
set.seed(2) #we can set different seeds each time until we catch the bug we want to fix
my_results <- random_subset_new(threshold_to_beat =mean(bind_rows( lapply(results_list, data.frame))[,1])  )

#you can look in myresults as you wish.
#you can also have your functions output a variety of different information, e.g. in the form of a list
#e.g. results(list(summary=summary(temporary_flfp), iteration_number=counter))

# Browser()####

# I expand this function, adding some bugs as well

random_subset_new <- function(threshold_to_beat=0){
  
  browser() #this is like a stop point that will allow us to investigate within the function environment
  
  this_intercept <- 0 
  intercepts <- vector("numeric",0)
  counter <- 0 
  
  while(this_intercept <=threshold_to_beat){
    counter <- counter+1
    
    this_sample <- sample(44671:100000, 5000, replace = T) 
    
    temporary_flfp <- flfp[this_sample,]
    
    simple_model <- lm(data=temporary_flfp, patr_values~ religious + age_gr +edu)
    this_intercept <-  tidy(simple_model)[1,2]
    intercepts <- c(intercepts, this_intercept)
    
    if ((counter %% 10) ==0){ 
      cat("\nIteration number is", counter) #
    }
    
  } 
  
  
  
  return(summary(temporary_flfp), counter) 
  
  
}

# Run the function again####

set.seed(2) 

my_results <- random_subset_new(threshold_to_beat =mean(bind_rows( lapply(results_list, data.frame))[,1])  )

# lapply() functions####

lapply(X=results_list, FUN = max) #it loops through items of a list in order OR for a dataframe, it loops through the columns.
#

#you can make your own function in the following way within lapply
lapply(X=results_list, FUN = function(x) abs(x[1] -simple_model$coefficients[1])) #x in this function will be each item in the list. or in this case, each row of coefficients. I take the first item, which is the intercepts... so I compare the new models' intercepts with the original simple model. 


# mapply()
# * mapply() is like lapply, except that it excepts multiple lists (of the same size), which you can interact, combine in any way you like. 

