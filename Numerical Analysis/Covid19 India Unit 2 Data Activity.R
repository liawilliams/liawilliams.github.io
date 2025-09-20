library(readr)
Covid19India <- read_csv("Documents/UNI/MSc Data Science/M2 - Numerical Analysis/Unit 1 - Types & Sources of Data & an Intro to R/Covid-19 India Dataset/Covid19IndiaJan20Mar20.csv")
View(Covid19India)

library(dplyr)

#Create frequency table to show daily reports of recoveries v no recoveries
  # Create binary variable has_recovery
Covid19India$has_recovery <- ifelse(Covid19India$Cured > 0, 1, 0)
  #Create freq table
table(Covid19India$has_recovery)
  #Convert to factor to create labels
Covid19India$has_recovery <- factor(
  Covid19India$has_recovery, 
  levels = c(0, 1),
  labels = c("No Recoveries", "Recoveries Reported")
)
table(Covid19India$has_recovery)

#Calculate percentages to show what proportion of reports inc recovery cases
prop.table(table(Covid19India$has_recovery)) * 100
