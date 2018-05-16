### Author: Kate Sabey    ###
### Date: May 14, 2018    ###
### Purpose: Programming  ###
### Data: West Nile Virus ###


###Load Packages###

library(ggplot2)
library(magrittr)


###Declare Functions###

mean <- function(x) {
  #Computes mean of sample set
  #Arguments: x: vector of values to be averaged
  s <- sum(x)
  n <- length(x)
  #Computation
  m <- s/n
  return(m)
}

standarderror <- function(x) {
  #Computes standard error of sample set
  #Arguments: x: vector of values on which to calculate standard error
  sdev <- sd(x)
  n <- length(x)
  #Computation
  se <- sdev/sqrt(n)
  return(se)
}


###Load Data###

wnv <- read.csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv", head=TRUE)
View(wnv)


###Perform Analysis###

#make "Year" a categorical column
wnv$Year <- as.factor(wnv$Year)

#histogram of total number of west nile virus cases by year and colored by state
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total)) +
  labs(x='Total Number of Cases', y='Frequency', title='West Nile Virus Cases',
       caption="Data from: https://diseasemaps.usgs.gov/")

#log transform total case number to account for highly skewed data distribution
wnv$logCases <- log(wnv$Total, base=10)     

#histogram of log-transformed total number of west nile virus cases by year and colored by state
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=logCases)) +
  labs(x='Logarithm of Total Number of Cases', y='Frequency', title='Log Transformed West Nile Virus Cases',
       caption="Data from: https://diseasemaps.usgs.gov/")

#second way of plotting log-transformed histogram
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=log(Total))) +
  labs(x='Logarithm of Total Number of Cases', y='Frequency', title='Log Transformed West Nile Virus Cases',
       caption="Data from: https://diseasemaps.usgs.gov/")

#calculates case fatality rate for each row
wnv$CaseFatalityRate <- wnv$Fatal/wnv$Total

#histogram of calculated case fatality rates
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=CaseFatalityRate)) +
  labs(x='Case Fatality Rate', y='Frequency', title='Case Fatality Rates of West Nile Virus',
       caption="Data from: https://diseasemaps.usgs.gov/")

#confirms that the total number of cases to confirm that the variable "Total" is the sum of the number of febrile cases, neuroinvasive cases, and other cases
wnv$Total == wnv$Fever + wnv$EncephMen + wnv$Other

#confirms using sum function
wnv$rowsums <- rowSums(wnv[,3:5])
wnv$Total == wnv$rowsums

#provides annual case count for each state rounded down to the nearest dozen
#use modulo operator to round to nearest interval (+ to round up, - to round down)
wnv$rounded <- wnv$Total-(wnv$Total%%12)

#calculates rounding errors for each row in a new column called "rounding error"
wnv$roundingerror <- wnv$Total-wnv$rounded

#calculates the total error as the sum of the rounding errors
totalerror <- sum(wnv$roundingerror)
totalerror
#totalerror = 1241

#calculates neuroinvasive disease rate
wnv$EncephMenRate <- wnv$EncephMen/wnv$Total

#subsets California data to calculate mean and standarderror of disease rate for this state
California <- subset(wnv, State %in% c('California'))
CA.mean <- mean(California$EncephMenRate)
CA.se <- standarderror(California$EncephMenRate)
CA.sd <- sd(California$EncephMenRate)

#subsets Colorado data to calculate mean and standarderror of disease rate for this state
Colorado <- subset(wnv, State %in% c('Colorado'))
CO.mean <- mean(Colorado$EncephMenRate)
CO.se <- standarderror(Colorado$EncephMenRate)
CO.sd <- sd(Colorado$EncephMenRate)

#subsets New York data to calculate mean and standarderror of disease rate for this state
NewYork <- subset(wnv, State %in% c('New York'))
NY.mean <- mean(NewYork$EncephMenRate)
NY.se <- standarderror(NewYork$EncephMenRate)
NY.sd <- sd(NewYork$EncephMenRate)

#adds standard deviation column to data frame for specified column
wnv$sd <- sd(wnv$EncephMenRate)

#uses ggplot to show neuroinvasive disease rate for these states as bar graph with error bars to show standard deviation
#add error bars to bar plot using "geom_errorbar()"
ggplot(data=subset(wnv, State %in% c('California', 'Colorado', 'New York'))) + 
  geom_bar(mapping=aes(x=State, y=EncephMenRate, color=State), stat="summary") +
  geom_errorbar(mapping=aes(x=State, ymin=EncephMenRate-sd, ymax=EncephMenRate+sd)) +
  labs(x='State', y='Neuroinvasive Disease Rate', title='Bar plot for WNV Neuroinvasive Disease Rate by State',
       caption="Data from: https://diseasemaps.usgs.gov/")

#every case less than longitude 98.5795 at center of United States is labeled as Eastern or otherwise Western
wnv$region <- ifelse(abs(wnv$Longitude)<98.5795, "Eastern", "Western")

#subsets Eastern data and calculates mean and standard error of case fatality rate
Eastern <- subset(wnv, region %in% c('Eastern'))
Eastern.mean <- mean(Eastern$CaseFatalityRate)
Eastern.se <- standarderror(Eastern$CaseFatalityRate)

#subsets Western data and calculates mean and standard error of case fatality rate
Western <- subset(wnv, region %in% c('Western'))
Western.mean <- mean(Western$CaseFatalityRate)
Western.se <- standarderror(Western$CaseFatalityRate)

#The average case fatality rate of west nile virus is higher in the Eastern United States than in the Western United States
ggplot(data=wnv) + 
  geom_bar(mapping=aes(x=State, y=CaseFatalityRate, color=region), stat="summary") + 
  labs(x='State', y='Neuroinvasive Disease Rate', title='Bar plot for WNV Neuroinvasive Disease Rate by State',
       caption="Data from: https://diseasemaps.usgs.gov/") + coord_flip()

#creates table for output data
tbl<-matrix(nrow=9,ncol=5)
colnames(tbl)=colnames(wnv[,c(2,6,7,11,1)])
tbl[,1]=paste(1999:2007)
View(tbl)
#loop over years and compute total number of reported cases, total number of fatal cases,
#average case fatality rate per year, and number of states reporting cases
for (i in 1:nrow(tbl)) {
  dat=wnv[wnv$Year==(1998+i),]
  tbl[i,2]=sum(dat$Total)
  tbl[i,3]=sum(dat$Fatal)
  tbl[i,4]=mean(dat$CaseFatalityRate)
  tbl[i,5]=nrow(dat)                      #places number of states recording cases in "States" column
}

#creates data frame for visualization
mode(tbl) = "numeric"
tbl.df <- data.frame(tbl)
View(tbl.df)

#converts Year to numeric for visualization
tbl.df$Year <- as.character(tbl.df$Year)

#plot how total number of reported cases changes over time
ggplot(data=tbl.df, mapping=aes(x=Year, y=Total, group=1)) + 
  geom_point() +
  geom_line() + 
  labs(x='Year', y='Total Number of Reported Cases', title='Line plot for Total Number of Reported WNV Cases by Year',
       caption="Data from: https://diseasemaps.usgs.gov/")
#the total number of reported WNV cases peaked in 2003 before rapidly declining in 2004 and generally plateaus around 3000 cases per year

#plot how number of fatal cases changes over time
ggplot(data=tbl.df, mapping=aes(x=Year, y=Fatal, group=1)) + 
  geom_point() +
  geom_line() + 
  labs(x='Year', y='Total Number of Fatal Cases', title='Line plot for Total Number of Fatal WNV Cases by Year',
       caption="Data from: https://diseasemaps.usgs.gov/")
#the number of fatal WNV cases peaked sharply in 2002 before declining to around 150 fatal cases per year

#plot how average case fatality rate changes over time
ggplot(data=tbl.df, mapping=aes(x=Year, y=CaseFatalityRate, group=1)) + 
  geom_point() +
  geom_line() + 
  labs(x='Year', y='Case Fatality Rate', title='Line plot for WNV Case Fatality Rate by Year',
       caption="Data from: https://diseasemaps.usgs.gov/")
#the average WNV case fatality rate peaked in 2001 before quickly declining to around 0.03, likely due to increased awareness, diagnostics, treatments, 
#prevention, etc. following this spike in mortality

#use help to determine proper usage of prop.test for testing equal proportions (to determine estimated rate and confidence interval)
?prop.test
#prop.test(x, n, p = NULL,
#         alternative = c("two.sided", "less", "greater"),
#         conf.level = 0.95, correct = TRUE)

#see also section specifies that "binom.test" may also be useful function to test hypotheses