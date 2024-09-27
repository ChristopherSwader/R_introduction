# Day 1 Getting Started: Intro to R. Swader####
##Swader: Intro to R####

## A series of numbers####
1:10
10:1
1:10 + 10:1

##Working Directories####
getwd() #get the current direcotry
setwd("C:/your full path") #change this to your full path to your working directory or use the menu in the file window

##Basic R syntax####

my_dist <- rnorm(n=100, mean=0, sd=1)

?rnorm() #learn about a function
hist(my_dist)        #plot a simple histogram (non-fancy version)

my_dist <-rnorm(n=10, mean=0, sd=1) #a smaller sample size
hist(my_dist)

my_dist <-rnorm(n=1000, mean=0, sd=1) #a larger sample size
hist(my_dist)

my_dist <-rnorm(n=1000, mean=0, sd=1) 
hist(my_dist, breaks = 50)#specify more breaks

## A few other function examples####

Sys.Date()

sqrt(9)

seq(1, 10)

paste0("Swader", paste0(sample(1:10, 5, replace = F,),  collapse = ""))

##Install packages####
install.packages("wbstats")


##Load packages####
library(wbstats)

##Some basic commands####
"here is some text"
my_text <- "some important text" #assign something to an object


my_text <-  c(my_text,"Welcome", "User" ) # a string vector

##Argument names and order####
seq(to=10, from=1) #this is the same as the following. When specifying argument names, i can put them in any order
seq(1, 10) #with no argument names specified, they must be in the correct order

seq(1, 10, 1)
seq(10, 1, 1) #this generates an error because it is giving a sequence backwards, which means that 'by' should be -1
seq(10, 1, by = -1)
seq(to = 10, from = 1, by = 1)

##Optional arguments####
log(100, base = 10)
log(100) #base is optional because it has a default
log(base = 100) #the argument x is not optional, as no default is supplied

##Square brackets and sample()####
these_names <- c("Anna", "Bob", "Cindy", "David", "Edith")
these_names
this_draw <- sample(these_names, 10, replace = T)
this_draw
this_draw[9] #this takes the 9th item in this vector

##Object Assignment and rnorm()####
numbers <- rnorm(100, mean = 100, sd = 50)

value <- 10

#we can plug these objects directly into functions
mean(numbers)
sqrt(value)

##Your turn####
data(iris)
this_sample <-  sample(1:nrow(iris), round(.5*nrow(iris)), replace=F)
sub_iris <-  iris[this_sample,] 

#one can graph this as well. Just a snapshot of what we will do tomorrow.
library(ggplot2)
ggplot(sub_iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point()

## Datasets included in R####
data()
iris
head(iris)
View(iris)
?iris

##Independence of data objects####
data <- rnorm(100, mean = 100, sd = 15) 
copy <- data
data <- seq(1, 10, by = 1)
data
copy

##Data types####
my_numeric<- c(1, -3, 7.18)
typeof(my_numeric)
my_string <- c("apple", "banana")
typeof(my_string)
my_integers <- c(1L, 2L, 3L)
typeof(my_integers)
my_logicals <- c(TRUE, FALSE, FALSE)
typeof(my_logicals)

##Single values and Vectors####
this_value <- 10
this_vector <- 1:10
this_other_vector <- c("Marie","Pontus", "Georgina")

c(1, 2, 3) #also a vector
c(a = 1, b = 2, c = 3)#named vector

##Your turn####
vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
vec <- 1:10
vec <- seq(1, 10, by = 1)
vec

vec[11] <- "hello"
#vec  <- append(vec, ”hello”) #another option to add the next item
#vec  <- c(vec, ”hello”) # a further option
str(vec) 

#notice many functions will not recognize a vector of strings as numbers, even if the strings contain numbers!
mean(vec[1:10]) #that is why you get an error here even when removing "hello"

typeof( c(3.14, TRUE, 6L, "hello") ) #another example of type conversion

#square brackets again
vec[3]
vec[c(1,2,11)]
vec2 <- c(alpha = 1, beta = 2, gamma = 3)

##Your turn####
vec2["gamma"]
vec2[3]
vec2[c("alpha", "gamma")]

##Vectorized operations####
c(1, 2, 3, 4, 5) + c(1, 2, 3, 4, 5)
1 + c(1, 2, 3, 4, 5)

##Square brackets for dataframes####
str(iris)
iris[1,]
iris[,1]

##Your turn####
one <- 1
log(1)
log("1")
log("one")
log(one)

##Lists####
a_list <- list(numbers = 1:5,
               strings = "hello", 
               logicals = FALSE)
a_list
#extracting from a list
a_list$numbers
a_list$strings
a_list$logicals


a_list[[2]]
a_list[["numbers"]]

##Dataframes####
nums <- c(1, 2, 3, 4)
logs <- c(TRUE, TRUE, FALSE, TRUE)
strs <- c("apple", "banana", "carrot", "duck")

df <- data.frame(numbers = nums, 
                 logicals = logs, 
                 strings = strs)
df
df$strings
df[,"strings"]

##Factors####
#unordered
eyes <- factor(x = c("blue", "brown", "green", "green"),levels = c("blue", "brown", "green"))

##Your turn####
df$fruit <- factor(df$strings,
                    levels = c("carrot", "apple", "banana"))

##Ordered factor####
df$pro_choice <- factor(c("agree", "disagree", "agree", "strongly disagree"), ordered=TRUE, 
                         levels = c("strongly disagree", "disagree", "neutral", "agree", "strongly agree"))


#CONTINUE####