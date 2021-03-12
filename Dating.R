rm(list=ls())
setwd("C:/Users/dougr/Contacts/Desktop/Analytics Communication and Visualization/Assignments/Midterm Project")
library(dplyr)
library(readxl)
library(car)
library(tidyr)

dating <- read.csv("Dating.csv")
Wave1 <- filter(dating,dating$wave==1)

summary(dating)
str(dating)

dating$iid <- factor(dating$iid,ordered = T)
dating$id <- factor(dating$id,ordered = T)
dating$idg <- factor(dating$idg,ordered = T)
dating$gender <- factor(dating$gender, levels=c(0,1),
                      labels=c("Female","Male"), ordered=F)
dating$wave <- factor(dating$position, levels=c(1:21), ordered=T)
dating$round <- factor(dating$round, levels=c(1:22), ordered=F)
dating$position <- factor(dating$position, levels=c(1:22), ordered=F)
dating$order <- factor(dating$order, levels=c(1:22), ordered=F)
dating$pid <- factor(dating$pid,ordered = F)
dating$match <- factor(dating$match, levels=c(0,1),
                     labels=c("No","Yes"), ordered=F)
dating$samerace <- factor(dating$samerace, levels=c(0,1),
                       labels=c("No","Yes"), ordered=F)
dating$age_o <- factor(dating$age_o,ordered = F)
dating$race_o <- factor(dating$race_o, levels=c(1:6), 
                      labels=c("Black/African American",
                               "European/Caucasian-American",
                               "Latino/Hispanic American",
                               "Asian/Pacific Islander/Asian-American",
                               "Native American",
                               "Other"), ordered=F)
dating$race <- factor(dating$race, levels=c(1:6), 
                    labels=c("Black/African American",
                             "European/Caucasian-American",
                             "Latino/Hispanic American",
                             "Asian/Pacific Islander/Asian-American",
                             "Native American",
                             "Other"), ordered=F)
dating$imprace <- factor(dating$imprace, levels=c(1:10), ordered=T)
dating$imprelig <- factor(dating$imprelig, levels=c(1:10), ordered=T)
dating$goal <- factor(dating$goal, levels=c(1:6), 
                    labels=c("Seemed like a fun night out",
                             "To meet new people",
                             "To get a date",
                             "Looking for a serious relationship",
                             "To say I did it",
                             "Other"),ordered=T)
dating$date <- factor(dating$date, levels=c(1:7), 
                    labels=c("Several times a week",
                             "Twice a week",
                             "Once a week",
                             "Twice a Month",
                             "Once a month",
                             "Several times a year",
                             "Almost never"),ordered=T)
dating$go_out <- factor(dating$go_out, levels=c(1:7), 
                    labels=c("Several times a week",
                             "Twice a week",
                             "Once a week",
                             "Twice a Month",
                             "Once a month",
                             "Several times a year",
                             "Almost never"),ordered=T)
dating$sports <- factor(dating$sports,levels=c(1:10),ordered=T)
dating$tvsports <- factor(dating$tvsports,levels=c(1:10),ordered=T)
dating$exercise <- factor(dating$exercise,levels=c(1:10),ordered=T)
dating$dining <- factor(dating$dining,levels=c(1:10),ordered=T)
dating$museums <- factor(dating$museums,levels=c(1:10),ordered=T)
dating$art <- factor(dating$art,levels=c(1:10),ordered=T)
dating$hiking <- factor(dating$hiking,levels=c(1:10),ordered=T)
dating$gaming <- factor(dating$gaming,levels=c(1:10),ordered=T)
dating$clubbing <- factor(dating$clubbing,levels=c(1:10),ordered=T)
dating$reading <- factor(dating$reading,levels=c(1:10),ordered=T)
dating$tv <- factor(dating$tv,levels=c(1:10),ordered=T)
dating$theater <- factor(dating$theater,levels=c(1:10),ordered=T)
dating$movies <- factor(dating$movies,levels=c(1:10),ordered=T)
dating$concerts <- factor(dating$concerts,levels=c(1:10),ordered=T)
dating$music <- factor(dating$music,levels=c(1:10),ordered=T)
dating$shopping <- factor(dating$shopping,levels=c(1:10),ordered=T)
dating$yoga <- factor(dating$yoga,levels=c(1:10),ordered=T)
dating$exphappy <- factor(dating$exphappy,levels=c(1:10),ordered=T)


Wave1 <- filter(dating,dating$wave==1)

  
#dating Correlations
#pairs(sports~tvsports+exercise+dining+concerts,data=dating)

#plot(y=Cars$mpg,x=Cars$origin)
