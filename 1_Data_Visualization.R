### Author: Kate Sabey          ###
### Date: May 14, 2018          ###
### Purpose: Data Visualization ###
### Data: MERS                  ###


#set working directory as path to downloaded csv file by using the Session drop down menu and selecting "Working Directory"
#or use setwd("C:/Users/workshop/Desktop/R")

#reads the .csv file and assigns it to "mers"
mers <- read.csv('cases.csv')

#use to view data
View(mers)

#use to inspect the first few rows of data
head(mers)

#shows that "onset" is a categorical or "factor" column
class(mers$onset)

#changes row 890 in hospitalized column to this value
mers$hospitalized[890] <- c('2015-02-20')

#removes row 471 from the data set
mers <- mers[-471,]

#installs the lubridate package
install.packages("lubridate")

#uses the lubridate package to reformat column class
library(lubridate)
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
#now shows as date

#the "na.omit" function omits any "NA" data values when determining the minimum value in the specified column
#without this function, the earliest onset date cannot be defined
day0 <- min(na.omit(mers$onset2))

#the "as.numeric" command changes the specified column class to numeric rather than as a categorical factor
mers$epi.day <- as.numeric(mers$onset2 - day0)

#shows what value of this assignment is
day0

#loads the ggplot2 package
library(ggplot2)

#epidemic curve (or bar plot) of MERS data
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#if the "+" convention is not used in the code to build a plot, the layers of the plot will not be properly added and the resulting plot 
#space will be blank (or only illustrate features prior to the missing "+")

#modifies the above epidemic curve to color bars according to country, illustrating geographic case distribution throughout the epidemic
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#modifies the plot so that each bar stack is of a standardized height (at 1.0), y-axis changes to proportions instead of counts
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#flips x and y axes
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_flip()

#uses polar coordinates on which to plot data
#good way to show seasonality!
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_polar()

#calculates the "raw" infectious period for each row as the difference between the time of hospitalization and the time of onset
mers$infectious.period <- mers$hospitalized2-mers$onset2

#specifies that these data are of the class "difftime"
class(mers$infectious.period)

#converts the class of this column to be numeric in units of days
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

#plots a histogram of infectious periods, showing that most individuals have a very short infectious period, while very few are of longer duration
#warning message suggests that can use 'binwidth' to choose better bin value
ggplot(data=mers) + 
  geom_histogram(aes(x=infectious.period)) + 
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#the previous code results in negative infectious period values as a result of nosocomial infections (onset following hospitalization)
#to correct for this, this code calculates infectious periods in the case where it is positive and otherwise sets it as zero
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)

#histogram plot of corrected MERS infectious period values, containing only positive values
ggplot(data=mers) + 
  geom_histogram(aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#to investigate the frequency of hospital-acquired MERS infections, could retain negative infectious period values
mers$infectious.period3 <- ifelse(mers$infectious.period>0,0,mers$infectious.period)

#then take absolute value of negative values before plotting
mers$abs.infectious.period3 <- abs(mers$infectious.period3)

ggplot(data=mers) + 
  geom_histogram(aes(x=abs.infectious.period3)) + 
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period (nosocomial infections only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#histogram plot of corrected MERS infectious period values, containing only values for hospital-acquired *nosocomial) infections

#density plot showing proportions of postive MERS infectious period values
ggplot(data=mers) + 
  geom_density(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#area plot showing frequency of postive MERS infectious period values
ggplot(data=mers) + 
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#dot plot showing frequency of postive MERS infectious period values
ggplot(data=mers) + 
  geom_dotplot(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', title='Dot plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#bar plot showing frequency of postive MERS infectious period values
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', title='Bar plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#plots corrected infectious period over time to study how infectious period changes over course of epidemic
ggplot(data=mers) + 
  geom_smooth(mapping=aes(x=epi.day, y=infectious.period2)) + 
  labs(x='Time', y='Infectious period', title='Change in infectious period over course of epidemic (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#around 5 days for about first half of epidemic before dropping and sharply rising towards end of epidemic
#it does appear as though societal learning occurs somewhat at around time point 1250, late in the epidemic
#the infectious period is reduced close to 1 at this point (indicating that isolating infected individuals likely helped to curtail outbreaks)
#however, this may have led to a false impression of outbreak decline and loosened policy since the infectious period then sharply peaks shortly after

#removes countries without positive values (don't need to do this for future analysis)
countryomit <- mers[mers$country == c('South Korea', 'KSA', 'UAE', 'Qatar', 'Jordan'),]
View(countryomit)

#plot corrected infectious period over time to study how infectious period changes over course of epidemic
#includes separately colored smooth fit line for each country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2, color=country)) + 
  geom_smooth(method="loess") +
  labs(x='Time', y='Infectious period', title='Change in infectious period over course of epidemic (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#facets plot of infectious period (positive) values by country (i.e. creates multi-panel plot)
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#subsets data and recreates above plot to exclude countries that didn't report a high number of cases and that included unusual gender codings
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping=aes(x=epi.day, y=infectious.period2, color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
    labs(x='Epidemic day', y='Infectious period',
    title='MERS infectious period by gender and country',
    caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#installs ggplot2 extension
install.packages("plotly")
library(plotly)

#creates interactive version of original bar plot/epidemic curve, rendered in HTML
#can extract more information from plot by selecting or hovering over plot regions
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)


