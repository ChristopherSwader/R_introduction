#Day 2####
##Swader: Intro to R####

#Import Data####
library(readr)
moral_issues <- read_csv("gss-moral-issues.csv")
View(moral_issues)
moral_issues

##Change data.frame format####
#what kind of data.frame is it?
str(moral_issues)
#or
class(moral_issues)
#i like pure data.frames, so I convert it to one.
moral_issues <- as.data.frame(moral_issues)
str(moral_issues)

#look at first rows only in the console
head(moral_issues)

#convert to tibble
library(tidyverse)
moral_issues <- as.tibble(moral_issues)

##load libraries####

library(tidyverse)

#Export Data####
##Save csv####
write_csv(moral_issues, path = "moral_issues.csv")

#Clean Data####
##convert columns to different format####
#convert all columns
new_moral_issues <- type_convert(moral_issues)
moral_issues$issue <- as.factor(moral_issues$issue)

#or convert individual column
new_moral_issues$year <- as.integer(moral_issues$year)

#Select/Isolate columns/variables####
#Using tidyverse
select(moral_issues, issue, prop)
#Using square brackets
moral_issues[,c("issue", "prop")]

#with re-naming the columns
select(moral_issues, moral_issue=issue,proportion= prop)

new_moral_issues <- moral_issues[,c("issue", "prop")]
colnames(new_moral_issues) <- c("moral_issue", "proportion")


##Your turn####
new_moral_issues <- moral_issues[,c("issue", "n")]

##select() helpers####
data(mpg)
select(mpg, cty:class)
select(mpg, -c(cty, hwy))
select(mpg, starts_with("c"))
select(mpg, ends_with("y"))
select(mpg, contains("d"))
select(mpg, matches("^.{4}$"))
select(mpg, one_of(c("fl", "fuel", "Fuel")))
select(mpg, num_range("x", 1:5)) #returns nothing, because mpg does not have columns named x1, x2, x3, etc.

##Quiz####
select(moral_issues, -c(issue, year, n))
select(moral_issues, prop:prop_cons)
select(moral_issues, starts_with("prop"))
select(moral_issues, ends_with("prop"))


##select() vs $####
#compare

select(moral_issues, n) #is a data.frame with one column
moral_issues$n #is a vector

moral_issues %>% pull(n) #also produces a vector

#you can convert the vector back into the column if you need
as.tibble(moral_issues$prop)

##factor levels####
#first convert issue to a factor
moral_issues$issue <- as.factor(moral_issues$issue)
levels(moral_issues$issue)

#Create new columns####

moral_issues %>%
  mutate(percent = round(prop*100, 2))

moral_issues %>%
  mutate(percent = round(prop*100, 2), nper = round(percent))

##Your turn####
moral_issues <- moral_issues %>% 
  mutate(polr = prop_lib - prop_cons) %>% 
   arrange(desc(polr))

#Filter rows/cases####
#tidyverse
filter(moral_issues, issue == "marblk")
#square brackets
moral_issues[moral_issues$issue=="marblk",]
#invert it
moral_issues[!moral_issues$issue=="marblk",]
#or
moral_issues[moral_issues$issue!="marblk",]

##tip: convert logical to 0s and 1s####
x <- c(T, F, T, T, T)
x*1

#use %in% to extract a range of rows
my_issues <-  c("marblk", "libath", "fehome")
moral_issues[moral_issues$issue %in% my_issues,]

#remove NAs
filter(moral_issues, !is.na(prop_lib))


##Your turn####
filter(moral_issues,issue=="abany" , prop>=.40)

moral_issues[moral_issues$issue=="abany" & moral_issues$prop>=.40,]

##Common mistakes####
filter(moral_issues, issue = "marblk") #wrong
filter(moral_issues, issue == "marblk") #right
filter(moral_issues, issue == marblk) #wrong
filter(moral_issues, issue == "marblk") #right
filter(moral_issues, prop_lib == NA) #wrong
filter(moral_issues,is.na(prop_lib)) #right

##Filter with AND####

filter(moral_issues, issue == "marblk", year == 2018)
filter(moral_issues, issue == "marblk" & year == 2018)

##Filter with multiple boolean statements####
filter(moral_issues, (issue == "abany" | issue == "premarsx") & year==2018)

##Your turn####
filter(moral_issues, issue == "abany" | issue == "premarsx") #fix this code to  return only the rows that contain either "abany" or "premarsx" issue in 2018
filter(moral_issues, (issue == "marblk" | issue == "affrmact") & year == 2018)

##multiple tests####
filter(moral_issues, 1990 < year < 2000) #wrong
filter(moral_issues, year > 1990, year < 2000)

filter(moral_issues, issue == "marblk"|issue == "affrmact") #same as below
filter(moral_issues, issue %in% c("marblk", "affrmact"))

filter(moral_issues, (issue == "marblk" | issue == "affrmact") &     
         year == 2018) #same as below
filter(moral_issues, issue %in% c("marblk", "affrmact") & 
         year == 2018)

#Re-sort rows####
arrange(moral_issues, prop)
arrange(moral_issues, desc(prop))

#square bracket version
moral_issues[order(moral_issues$prop),]
moral_issues[order(moral_issues$prop, decreasing = T),]

#min and max of data
#best practice is
min(moral_issues$prop)
max(moral_issues$prop)
#or
summary(moral_issues$prop)

#The pipe####
##Iteration style####
issues_2018 <- filter(moral_issues, year == 2018)
issues_2018 <- select(issues_2018, issue, prop)
issues_2018 <- arrange(issues_2018, desc(prop))
issues_2018

##Nested parentheses style####
arrange(select(filter(moral_issues, year == 2018), issue, prop), desc(prop))

##Pipe Style####
moral_issues %>%
  filter(year == 2018) %>%
  select(issue, prop) %>%
  arrange(desc(prop))

##Your turn####
moral_issues %>%
  filter(year == 1974) %>%
  select(issue, prop) %>%
  arrange(prop)

#Converting between long and short data formats####

##Toy Data####
# Toy datasets to use

#pretend this is the number of sociologists at the national associations
##sociologist data####
cases <- tribble(
  ~Country, ~"2011", ~"2012", ~"2013",
  "FR",    7000,    6900,    7000,
  "DE",    5800,    6000,    6200,
  "US",   15000,   14000,   13000
)





##Pivot wider####
##pollution data####

pollution <- tribble(
  ~city,   ~size, ~amount,
  "New York", "large",      23,
  "New York", "small",      14,
  "London", "large",      22,
  "London", "small",      16,
  "Beijing", "large",      121,
  "Beijing", "small",      56
)

pollution %>% pivot_wider(names_from = size, values_from = amount)

###Your turn####
##Pivot wider####
moral_issues_wide <-
moral_issues %>%
  select(issue, year, prop) %>%
  filter(issue=="abany"|issue=="premarsx") %>%
  pivot_wider(names_from =   year, values_from =  prop)
##Pivot longer####
###Your turn####

cases %>% pivot_longer(2:4, names_to = "year", values_to = "n") #with column indices
cases %>% 
  pivot_longer(c("2011", "2012", "2013"), names_to = "year", values_to = "n") #with column names
cases %>% pivot_longer(-Country, names_to = "year", values_to = "n") #with everything except this column

moral_issues_longer <- 
  moral_issues_wide %>% 
  pivot_longer(2:31, 
               names_to = "year", 
               values_to = "prop")

