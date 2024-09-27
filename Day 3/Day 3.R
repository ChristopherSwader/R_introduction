#Day 3####
##Swader: Intro to R####

#Load Data and Libraries####
library(readr)
library(tidyverse)
moral_issues <- read_csv("C:/Users/soc-csw/Dropbox/Teaching/Introduction to R METHODS WORKSHOP/Chris/Day 2/gss-moral-issues.csv")


pollution <- tribble(
  ~city,   ~size, ~amount,
  "New York", "large",      23,
  "New York", "small",      14,
  "London", "large",      22,
  "London", "small",      16,
  "Beijing", "large",      121,
  "Beijing", "small",      56
)

#Aggregate statistics####
#tidyverse version
moral_issues %>%
  group_by(issue) %>%
  summarise(total = sum(n), max = max(n))
#base R version (tidyverse is more flexible here)
aggregate(moral_issues$n, by=list(issue=moral_issues$issue), sum)

##Your turn####
moral_issues %>%
  filter(issue == "abany") %>%
  summarise(mean_prop = mean(prop),
            first = min(year),
            last = max(year))

##How many rows?####
moral_issues %>% summarise(n = n()) #use this rather with group_by
nrow(moral_issues) #is much simpler if you need the rows in the whole dataset

##Distinct values####
moral_issues %>% summarise(n = n(),
                           n_issue = n_distinct(issue))
#base R version
length(unique(moral_issues$issue))

#Grouping cases####

#group by
pollution %>% 
  group_by(city)


#group by (Two variables) plus summarize
pollution %>% 
  group_by(city, size) %>%
  summarise(mean = mean(amount), sum = sum(amount), n = n())

##Your turn####
#group by plus summarize
pollution %>%
  group_by(city) %>%
  summarise(mean = mean(amount), sum = sum(amount), n = n())

##Ungroup####
pollution %>%
  group_by(city) %>%
  ungroup() %>%
  summarise(mean = mean(amount), sum = sum(amount), n = n())

#compare with
pollution %>% 
  group_by(city) %>%
  summarise(sum = sum(amount))

##Your turn####
moral_issues %>%
  group_by(issue) %>%
  summarise(mean_prop = mean(prop)) %>%
  arrange(desc(mean_prop))

##Descriptives####
###Numerical variables####

summary(moral_issues$prop)
mean(moral_issues$prop)
min(moral_issues$prop)
max(moral_issues$prop)
sd(moral_issues$prop)

###Categorical variables####
count(moral_issues, issue) #tidy way
table(moral_issues$issue) #base R way


##All variables: skimr####
install.packages("skimr")
library(skimr)
skim(moral_issues)

##Bivariate analysis####
###Correlations####
#basic functionality
cor(na.omit(moral_issues)[,-1])


install.packages("corrtable")
library(corrtable)
####with signif levels####
correlation_matrix(moral_issues, digits = 2 , use = "lower", replace_diagonal = T)
###Correlation Heatmaps####
#I pulled this code from online tutorials and applied it
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization






library(reshape2)
my_corr_matrix <- cor(na.omit(moral_issues)[,-1])
upper_tri <-my_corr_matrix*upper.tri(my_corr_matrix, diag = T)

melted_matrix <- melt(upper_tri)
library(ggplot2)
#basic version
ggplot(data = melted_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()
#fancier version

ggplot(data = melted_matrix, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()

##Bi and Multivariate modeling####
###Load data####
flfp <- readRDS("flfp-individual-level.rds")
flfp_agg <- readRDS("flfp-aggregated.rds")

flfp 
library(skimr)
skim(flfp)


###lm() function####


####Scatterplot with regression line####
library(ggplot2)
ggplot(flfp, aes(x = log_gdp, y = patr_mean)) +
  geom_point()+
  geom_smooth(method = "lm" )

####Running the model####
my_model <- lm(patr_mean ~ log_gdp, data = flfp)
my_model

summary(my_model)
#diagnostics
#fitted vs. residual plot
plot(my_model, which = 1)

##ggplot version
ggplot(my_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth()

#QQ plot
plot(my_model, which = 2)

##ggplot version
ggplot(my_model,aes( sample = .resid)) +
  geom_qq()

#broom package
library(broom)
tidy(my_model)
glance(my_model)
augment(my_model)

##Your turn####

issue_pairing <-  lm(prop_lib ~ prop_cons, data=moral_issues )
tidy(issue_pairing)
glance(issue_pairing)
plot(issue_pairing, which = 1)
plot(issue_pairing, which = 2)
glance(issue_pairing) %>% select(adj.r.squared, BIC)

#Multivariate regression####
mod_agg  <- lm(wvs_flfp ~ patr_mean + log_gdp, data = flfp_agg)
tidy(mod_agg )
glance(mod_agg) %>%
  select(adj.r.squared, BIC)

mod_agg_quadratic <- lm(wvs_flfp ~ patr_mean + log_gdp + I(log_gdp^2),
                data = flfp_agg)
tidy(mod_agg_quadratic)

#compare the two models
glance(mod_agg) %>%
  select(adj.r.squared, BIC)

glance(mod_agg_quadratic) %>%
  select(adj.r.squared, BIC)
#they are essentially the same... in fact the more complex model is a tiny bit worse.
  #but the lower BIC is (marginally) better

#Your turn####
mod_agg_patr2 <- lm(wvs_flfp ~ patr_mean + I(patr_mean^2) + log_gdp,
                data = flfp_agg)
tidy(mod_agg_patr2)

#compare
glance(mod_agg_patr2) %>%
  select(adj.r.squared, BIC) #this is the better model

glance(mod_agg_quadratic) %>%
  select(adj.r.squared, BIC)

#categorical predictors####
mod_reg <- lm(wvs_flfp ~ region, data = flfp_agg)
tidy(mod_reg)



#interaction terms####
mod_int <- lm(wvs_flfp ~ patr_mean*muslim, data = flfp_agg)
tidy(mod_int)

#glm()####
#fit a model for a binary outcome
flfp
skim(flfp)
model_val <- glm(lfp ~ patr_values + cntry,
                 family = binomial(link = "logit"),
                 data = flfp)
tidy(model_val) %>% filter(!str_detect(term, "cntry")) #we filter out cntry because we focus on patriarchical values and there are too many dummies to interpret for now

tidy(model_val, exponentiate = TRUE) %>%
  filter(!str_detect(term, "cntry")) #to get odds ratios

#Your turn####
#add country-level patriarchical values mean
mod_val_cntr <- glm(lfp ~ patr_values + patr_mean + cntry,
                    family = binomial(link = "logit"),
                    data = flfp)
tidy(mod_val_cntr, exponentiate = TRUE) %>%
  filter(!str_detect(term, "cntry"))

#control for categorical education
mod_val_edu <- glm(lfp ~ patr_values + edu + cntry,
                   family = binomial(link = "logit"),
                   data = flfp)
tidy(mod_val_edu, exponentiate = TRUE) %>%
  filter(!str_detect(term, "cntry"))

#regional model

mod_reg <- glm(lfp ~ region + edu + age_gr + marit +
                 children + religious + denom,
               family = binomial(link = "logit"),
               data = flfp)
tidy(mod_reg, exponentiate = TRUE)

#Your turn####
mod_reg <- glm(lfp ~ region + edu + age_gr + marit + 
                 children + religious + denom, 
               family = binomial(link = "logit"),
               data = flfp)
tidy(mod_reg, exponentiate = TRUE) 

##control for values####
mod_reg_val <- glm(lfp ~ region + patr_values + patr_mean +
                     edu + age_gr + marit + children +
                     religious + denom,
                   family = binomial(link = "logit"),
                   data = flfp)
tidy(mod_reg_val, exponentiate = TRUE)
