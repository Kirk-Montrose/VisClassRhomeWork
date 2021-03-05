library(dplyr)
library(ggplot2)
library(tidyr)


setwd("~/Documents/Github/VisClassRhomeWork/Data") #Set your on path
dataFromDating <- read.csv("dating.csv")
filterData <- filter(dataFromDating,wave ==2) #Get Data from first wave.
fit <- lm(wave~id , data = dataFromDating )
print(summary(filterData))
