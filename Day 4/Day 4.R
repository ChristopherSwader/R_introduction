#Day 4####
##Swader: Intro to R####

#Load Data####
setwd("C:/Users/soc-csw/Dropbox/Teaching/Introduction to R METHODS WORKSHOP/Chris/Day 4")

library(readr)
library(tidyverse)
moral_issues <- read_csv("C:/Users/soc-csw/Dropbox/Teaching/Introduction to R METHODS WORKSHOP/Chris/Day 2/gss-moral-issues.csv")
flfp <- readRDS("flfp-individual-level.rds")
flfp_agg <- readRDS("flfp-aggregated.rds")

pollution <- tribble(
  ~city,   ~size, ~amount,
  "New York", "large",      23,
  "New York", "small",      14,
  "London", "large",      22,
  "London", "small",      16,
  "Beijing", "large",      121,
  "Beijing", "small",      56
)

##Histograms and barcharts####
#continuous variables
ggplot(flfp, aes(patr_values)) +
  geom_histogram()

#categorical variables
ggplot(flfp, aes(denom)) +
  geom_bar()

##Scatterplots####

ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point()
#axis labels
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point()+
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values")
#regression line

ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point()+
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T)
#polynomials, curive
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point()+
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))
#color regions
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(colour = region))+
   labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))
#change stylistic theme of plot
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(colour = region))+
  theme_classic() +
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))

#new theme
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(colour = region))+
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))+
  theme_dark()

#new theme
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(colour = region))+
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))+
  theme_bw()


##Your turn####

ggplot(flfp_agg, aes(x=log_gdp, y=ilo_flfp)) +
  geom_point(aes(colour = region))+
  theme_light() +
  labs(y= "Female Lab.Force Part. (ILO)", x = "log GDP") +
  geom_smooth(se = T)+
  geom_text(data=filter(flfp_agg, cntry=="Russia"), aes(label=cntry))

#Mappings####
ggplot(flfp_agg) + 
  geom_point(aes(x = patr_mean, y = wvs_flfp))

#colour
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(colour = region))+
  theme_classic() +
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))

#size
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(size = log_gdp))+
  theme_classic() +
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))

#shape

ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(shape = region))+
  theme_classic() +
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))
#alpha
ggplot(flfp_agg, aes(x=patr_mean, y=wvs_flfp)) +
  geom_point(aes(alpha = log_gdp))+
  theme_classic() +
  labs(y= "Female Lab.Force Part. (WVS)", x = "Patriarchical Values") +
  geom_smooth(method = "lm", se = T, formula = y ~ poly(x, 3))

##Your turn####

#multiple aesthetics, discrete variable= color

ggplot(flfp_agg, aes(x=log_gdp, y=ilo_flfp)) +
  geom_point(aes(colour = region, size=log_gdp))+
  theme_light() +
  labs(y= "Female Lab.Force Part. (ILO)", x = "log GDP") +
  geom_smooth(se = T)+
  geom_text(data=filter(flfp_agg, cntry=="Russia"), aes(label=cntry))

#multiple aestthetics, discrete variable= size, continuous variable= color
ggplot(flfp_agg, aes(x=log_gdp, y=ilo_flfp)) +
  geom_point(aes(colour = log_gdp, size=region))+
  theme_light() +
  labs(y= "Female Lab.Force Part. (ILO)", x = "log GDP") +
  geom_smooth(se = T)+
  geom_text(data=filter(flfp_agg, cntry=="Russia"), aes(label=cntry))



#Set vs. mapping variables####

#map
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp, color = region))+
  theme_classic()

#set color
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp), color = "blue")+
  theme_classic()

##Your turn####
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp), color = "blue", shape=23, fill="lightblue", size=3)+
  theme_classic()

#Geoms####

#Your turn####
ggplot(flfp, aes(denom)) +
  geom_bar(aes(fill=denom))+
  theme_classic()

ggplot(flfp, aes(denom)) +
  geom_bar(aes(color=denom))+
  theme_classic()

##Stacked bar graph####
ggplot(flfp, aes(x=denom)) +
  geom_bar(aes(fill=region))+
  theme_classic()


#Boxplots####
#scatterplot by region
ggplot(flfp_agg) +
  geom_point(aes(x = region, y = wvs_flfp))+
  theme_bw()
##Your turn####
#boxplot
ggplot(flfp_agg) +
  geom_boxplot(aes(x = region, y = wvs_flfp))+
  theme_bw()
#remember help!
?geom_histogram()

#Your turn####
#multiple geoms, same plot
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp))+
  geom_smooth(aes(x = patr_mean, y = wvs_flfp))+
  theme_bw()

##Global mapping####

ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point()+
  geom_smooth()+
  theme_bw()

##Local mapping####
ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point(aes(colour = region)) +
  geom_smooth()+
  theme_bw()
#data can be set locally in the geom instead of globally
ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point(aes(colour = region)) +
  geom_smooth(data = filter(flfp_agg, region == "mena"))+
  theme_bw()

##Geom-based aesthetics####
ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point(aes(colour = region)) +
  geom_smooth(aes(linetype = region),se = FALSE, method = "lm")+
  theme_bw()

##Your turn####
##Scatterplot with labels

ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point(aes(colour = region)) +
  geom_text(aes(label = code))+
  theme_bw()

##fix labels so they don't overlap####
library(ggrepel)
ggplot(flfp_agg, aes(x = patr_mean, y = wvs_flfp)) +
  geom_point(aes(colour = region)) +
 # geom_text(aes(label = code))+
  theme_bw()+
  geom_text_repel(aes(label = code), max.overlaps = 57)

#What else####
#Facets####
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp)) +
  facet_wrap(vars(region))+
  theme_bw()

#Position adjustments####
#default position of bars
ggplot(flfp_agg) +
  geom_bar(aes(x = region,
               fill = oil_gr),
           position = "stack")+
  theme_bw()

#adjusted position of bars
ggplot(flfp_agg) +
  geom_bar(aes(x = region,
               fill = oil_gr),
           position = "dodge")+
  theme_bw()

#Scales####
ggplot(flfp_agg) +
  geom_point(aes(x = patr_mean, y = wvs_flfp, colour = region)) +
  scale_color_viridis_d()+
  theme_bw()

#Save plot####
ggsave("my_graph.pdf")

#intearctive graphics with plotly####
library(plotly)

d <- diamonds[sample(nrow(diamonds), 1000), ]

fig <- plot_ly(
  d, x = ~carat, y = ~price,
  # Hover text:
  text = ~paste("Price: ", price, '$<br>Cut:', cut),
  color = ~carat, size = ~carat
)

fig

##Your turn####

pl <- ggplot(flfp_agg, aes(x = patr_mean,
                           y = wvs_flfp,
                           colour = region,
                           label = cntry)) +
  geom_point()


ggplotly(pl)

#animated plots with gganimate####
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')#+
#  geom_smooth(method = "lm")

# Save as gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")
##Your turn####
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear') +
  geom_smooth(method = "lm")


#!Switch to rmarkdown script file####

