library(dplyr)
library(ggplot2)
library(tidyr)


setwd("/Users/kirkmontrose/Desktop/") #/Change this to point at you own location 
dataFromDating <- read.csv("dating.csv")

filterData <- filter(dataFromDating,wave ==1) #Get Data from first wave.

fit <- lm(wave~id , data = dataFromDating )


print(summary(filterData))
