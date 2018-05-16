### Author: Kate Sabey  ###
### Date: May 15, 2018  ###
### Purpose: Wrangling  ###
### Data: Lyme Disease ###


###Load Packages###

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)
library(devtools)


###Declare Functions###

#creates full fips code for each county
fips.builder <- function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

#can also use "paste0" instead of "sep" option to not have separator when paste


###Load Data###

ld <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/lyme.csv")
View(ld)

pop <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/pop.csv")
View(pop)

#the pop tibble fails to conform to tidy data format because:
#the column "areaname" contains data that isn't consisten among each row (i.e. some contain only states or counties while others contain both)
#the first row pertains to the entire United States rather than pertaining to the specific values in other rows (i.e. not consistent) 
#doesn't have unique column for all unique values (i.e. "pop20104" and "base20104" contain same values)
#the "county_fips" column is not consistently padded to 3 digits
#columns break up continuous data set (i.e. column for each year interspersed so not in order)

prism <- read_csv(file = "https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/climate.csv")
View(prism)


###Perform Analysis###

#retains the "fips" column and any columns whose header begins with "pop2"
pop %<>% select(fips,starts_with("pop2"))

#takes multiple year columns and collapses them into a single column, places corresponding values in a "size" column, and omits NA's
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit

#creates a "year" column that contains the year values in the "str_year" column without the preceding "pop" (the number only)
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))

#changes the "year" column to the integer class
pop %<>% mutate(year=as.integer(year))

#removes the 0 at the beginning of each value in the "flips" column
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))

#changes the "fips" column to the integer class
pop %<>% mutate(fips=as.integer(fips))

#pop tibble is now in tidy format
#to remove state-wide population summaries (in "fips" column), could use: pop <- filter(pop,fips %%100 !=0)
#to remove "str_year" column, could use: pop %<>% select(fips,size,year)


#collapsed case columns, putting years in a new column called "str_year" and the case values in a new row called "cases"
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")

#creates a "year" column that contains the year values in the "str_year" column without the preceding "Cases" (the number only)
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))

#changes the "year" column to the integer class
ld %<>% mutate(year=as.integer(year))

#renames the "STNAME" column to "state" and the "CTYNAME" column to "county"
ld %<>% rename(state=STNAME,county=CTYNAME)

#applies the "fips.builder" function
#use "rowwise" so will match row 1 values for each passed column 
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))

#retains all columns except the "STCODE", "CTYCODE", and "str_year" columns
ld %<>% select(-c(STCODE,CTYCODE,str_year))

#ld tibble is now in tidy format


#join prism and ld data frames into a new data frame, retaining county-year combinations for which have both disease and climate data
ld.prism <- inner_join(ld,prism)
View(ld.prism)

#join pop, ld, and prism data frames into a new data frame, retaining county-year combinations for which have disease, climate, and demographic data
pop.ld.prism <- inner_join(ld.prism,pop)
View(pop.ld.prism)

#create a new data frame to determine how many cases of Lyme disease were reported each year
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
cases_by_year
#the year with the highest number of cases was 2009

#create a new data frame to determine the average number of Lyme disease cases in each state
average_cases_by_state <- ld %>% ungroup %>% group_by(state) %>%
  summarize(average_cases=mean(cases)) %>% arrange(desc(average_cases))
average_cases_by_state
#the three states with the highest average number of cases are Connecticut, Massachusetts, and New Jersey, respectively.


#saves data frame as rda file, saves to "Documents"
#can load with: load("pop.ld.prism.rda")
save(pop.ld.prism, file = "pop.ld.prism.rda")

#saves written csv file from data frame to "Documents"
write_csv(pop.ld.prism, "pop.ld.prism.csv")


#get map data for United State counties and states
county_map <- map_data("county")
state_map <- map_data("state")

#groups the "pop.ld.prism" data frame by fips
ag.fips <- group_by(pop.ld.prism,fips)
View(ag.fips)

#summarizes the number of cases associated with each fips number in a new column called "all.cases"
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
View(ld.16y)

#according to fips, joins the "state" and "county" columns from "pop.ld.prism" to the "ld.16y" data frame
#left designation forces to join according to left column
ld.16y<-left_join(select(pop.ld.prism,c(state,county,fips)),ld.16y)

#retains only distinct rows
ld.16y<-distinct(ld.16y)

#renames the "state" column to be called "region" and the "county" column to be called "subregion"
ld.16y %<>% rename(region=state,subregion=county)

#removes the "County" designation in all values where present in the "subregion" column
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")

#changes all of the text in the "region" column to lowercase
ld.16y$region<-tolower(ld.16y$region)

#changes all of the text in the "subregion" column to lowercase
ld.16y$subregion<-tolower(ld.16y$subregion)

#cacluates the log values for the case values and puts them in a new column called "log10cases"
ld.16y %<>% mutate(log10cases=log10(1+all.cases))

#according to the region and subregion columns, joins the geographic data in the "county_map" frame with the "ld.16y" data frame
map.ld.16y<-left_join(county_map,ld.16y)
View(map.ld.16y)

#visualizes map data
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))


###Notes###

#full_join uses column in common to combine those that don't
#select is for columns and filter is for rows 
#map links: https://github.com/esdarling/Misc-R-code/blob/master/ggmap%20Kenya%20code.R
#           https://www.lytonsanalytics.com/2017/10/05/using-r-to-draw-different-geographical-maps/
  