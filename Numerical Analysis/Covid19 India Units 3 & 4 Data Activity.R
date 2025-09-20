#Load the data 
Covid19IndiaJan20Mar20 <- read_csv("~/Documents/UNI/MSc Data Science/M2 - Numerical Analysis/Unit 1 - Types & Sources of Data & an Intro to R/Covid-19 India Dataset/Covid19IndiaJan20Mar20.csv")

#load dplyr
library(dplyr)

#Create binary variable has_deaths
Covid19IndiaJan20Mar20$has_deaths <- ifelse(Covid19IndiaJan20Mar20$Deaths > 0,1,0)

#Create frequency table - daily reports of recoveries vs no recoveries
table(Covid19IndiaJan20Mar20$has_deaths)
head(Covid19IndiaJan20Mar20[, c("Deaths", "has_deaths")])

#Create factor variable with labels
Covid19IndiaJan20Mar20$has_deaths <- factor(
  Covid19IndiaJan20Mar20$has_deaths,
  levels=c(0,1), 
  labels=c("No Deaths", "Deaths Reported")
)
table(Covid19IndiaJan20Mar20$has_deaths)

#Create categorical variable case_level
  #Compute total confirmed cases
Covid19IndiaJan20Mar20$total_cases <- Covid19IndiaJan20Mar20$ConfirmedIndianNational + Covid19IndiaJan20Mar20$ConfirmedForeignNational
  #Create categorical variable
Covid19IndiaJan20Mar20$case_level <- cut(
  Covid19IndiaJan20Mar20$total_cases,
  breaks=c(-Inf,0,5,15,Inf),
  labels=c("No Cases", "Low Cases", "Medium Cases", "High Cases")
)
  #Check variables
table(Covid19IndiaJan20Mar20$case_level)
head(Covid19IndiaJan20Mar20[, c("total_cases", "case_level")])

#Create frequency table of reports per state
table(Covid19IndiaJan20Mar20$`State/UnionTerritory`)
  #Sort table in descending order
state_freq <- sort(table(Covid19IndiaJan20Mar20$`State/UnionTerritory`), decreasing=TRUE)
  #View sorted table
state_freq

#View top 10 states by daily reports
top10states <- head(state_freq, 10)
top10states

