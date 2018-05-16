### Author: Kate Sabey  ###
### Date: May 15, 2018  ###
### Purpose: Modeling   ###
### Data: Lyme Disease  ###


####Load Packages####

library(tidyverse)
library(magrittr)
library(GGally)
library(dplyr)
library(ggplot2)
library(modelr)


####Declare Functions####

#takes data frame as argument and returns linear model object predicting size (y) by year (x)
linGrowth_model <- function(df){
  lm(size~year, data=df)
}

#accepts object (such as values in resids list) and returns sum of absolute value
sum_resids <- function(x){
  sum(abs(x$resid))
}

#accepts linear model as argument and returns slope
get_slope <- function(model){
  model$coefficients[2]
}

#accepts data frame as argument and returns spearman correlation coefficient between Lyme disease cases and precipitation
#prevent R from repeatedly warning about computing p-values with ties
#tag at end so pull out correlation coefficient
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}


####Load Data####

setwd("C:/Users/workshop/Desktop/R")
ld.prism.pop <- read_csv(file = "pop.ld.prism.csv")
View(ld.prism.pop)


####Perform Analysis####

#creates summary plot for each of specified columns
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))
    #used to show range of data, which kinds of values are rare and common, and whether data are correlated with each other
    #main diagonals will display the density
    #main diagonals display density of the data (like a histogram, but continuous rather than binned)
    #the lower triangle plots show the correlation between each pair of data
    #the upper triangle will report the correlation coefficient

#size and cases density data are clumped so transform to log scale for visualization
ld.prism.pop %<>% mutate(log10size=log10(size))
ld.prism.pop %<>% mutate(log10cases=log10(1+cases))
    #add 1 to each case value since can't take the log of 0 but can take the log of 1 (which equals 0)
    #this way, original 0 values are still 0 upon log transformation

#creates summary plot including new log transformed column
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases"))
    #density data now less clumped for transformed columns due to log scale

#ggpairsplot suggests precipitation and average temperature are correlated
#investigate this using random subset of data
#reproducibly creates random subset of 100 rows
set.seed(222)
sample.100 <- ld.prism.pop %>% sample_n(100)
View(sample.100)

#plots precipitation versus average temperature of random sample
MyPlot <- ggplot(sample.100) +
  geom_point(aes(prcp,avtemp))

#calls plot assigned above to add subsequent layers 
MyPlot 

#adds best fit line to plot
MyPlot + geom_smooth(aes(prcp,avtemp), method="lm")

#creates a linear model (lm) object for the subsetted data, store the model and summarize
MyModel <- lm(avtemp~prcp,data=sample.100)
summary(MyModel)

#extract information from model object, access slope and associated p-value
summary(MyModel)$coefficients[2,1]
summary(MyModel)$coefficients[2,4]
    #slope of plotted line is 0.006720321 and p-value is 3.190341e-06, so the slope is signficantly different from 0 (p<0.05)

#generates ggplot of total population size by year
#the "." passes the new (grouped) unnamed datat frame to ggplot
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.) + geom_point(aes(x=year,y=total))

#to determine if all states are contributing equally to this growth
#creates new tibble called "by_state" that is groups the main data frame by state
by_state <- ld.prism.pop %>% group_by(state)
by_state

#updates new tibble so that it is nested
#lists number of rows for each state in a list-column called "data"
by_state %<>% nest
by_state

#displays Georgia data in the console window (Georgia is the 10th state in the list)
by_state$data[[10]]
    #this is itself a data frame, 1 row for each county
    #list elements are accessed with [[]]

#uses purrr (part of tidyverse) to perform state-wise modeling
models <- purrr::map(by_state$data, linGrowth_model)
    #to ensure calling map function of tidyverse library versus map function of map library (if both loaded):
    #can unload latter with: detach("package:maps", unload=TRUE)
    #or can clarify function call by preceding with package name and 2 colons, maps::map or purrr::map

#adds a column to the "by_state" data frame where each state (row) has its own model object
#linear model object created for each state's particular data frame
#purrr::map takes a vector as input and applies a function to each element of the vector
by_state %<>% mutate(model=purrr::map(data, linGrowth_model))

#adds residuals (difference between model prediction and actual data)
#purrr::map2 takes two arguments and creates new data from them
by_state %<>% mutate(resids=purrr::map2(data, model, add_residuals))

#resids is also column-list, so that each data value is itself a data frame with a "resid" column
#in which residual value is listed for each county
by_state
by_state$resids[[10]]

#adds column to "by_state" that contains total residual value for each state (summed over counties and years)
by_state %<>% mutate(TotalResid=purrr::map(resids, sum_resids))
by_state

#adds column to "by_state" that contains the lm slope for each state (creates another list-column)
by_state %<>% mutate(slope=purrr::map(model, get_slope))
by_state

#un-nests structure into new data frames for visualization of slope and residuals in ggplot
slopes <- unnest(by_state, slope)
TotalResids <- unnest(by_state, TotalResid)
slopes
TotalResids

#plots how growth rate varies by state
#rotates x-axis labels to be vertical
slopes %>% ggplot(aes(state, slope)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plots the total residuals by state
#rotates x-axis labels to be vertical
TotalResids %>% ggplot(aes(state, TotalResid)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #a residual value close to 0 indicates that the state's growth rate is well-described by the linear model (i.e. District of Columbia and Vermont)

#creates new tibble called "by_state2" that is groups the main data frame by state
by_state2 <- ld.prism.pop %>% group_by(state)
by_state2

#updates "by_state2" so that it is nested
#lists number of rows for each state in a list-column called "data"
by_state2 %<>% nest
by_state2

#adds a "spCor" column to the "by_state2" data frame that contains the correlation cofficients
by_state2 %<>% mutate(spCor=purrr::map(data, runCor))

#un-nests structure into new data frame for visualization of correlation coefficients
spCors <- unnest(by_state2,spCor)

#arranges correlation coefficients in descending order
spCors %<>% arrange(desc(spCor))

#changes the state column to a factor
spCors$state <- factor(spCors$state, levels=unique(spCors$state))

#plots correlation coefficient by state to show in which states the number of Lyme Disease cases is most correlated with precipitation
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #highest correlation in Maine and New York


####Notes####

#GGally: correlations between columns of data frame
#sample_n(number of elements from df): random sample of x values
#to make work reproducible, set.seed(#) - no matter how many times sample, will get same value every time
#geom_smooth with linear model or "lm" as method
#summary(lm), gives characteristic statistics of how good of a fit the model is
#use list to extract coefficients of interest
#cor.test: gives correlation coefficients
#modelr package is set of functions to help seamlessly integrate modeling in data manipulation and visualization pipeline
#Cntl-Shift-C comments on and off
#Note package versions in code script for reproducibility 
