rm(list=ls())
where git
setwd("C:/Users/dougr/Contacts/Desktop/Analytics Communication and Visualization/Assignments/Midterm Project")
library(dplyr)
library(readxl)
library(car)
library(tidyr)
library(purrr)

Wave1a <- filter(read.csv("Dating.csv"),read.csv("Dating.csv")$wave==1)
Wave1 <- Wave1a[,colSums(is.na(Wave1a))<nrow(Wave1a)] #remove all columns where all rows are NA ie. attr4_1...)

summary(Wave1)
str(Wave1)

Wave1$iid <- factor(Wave1$iid)
Wave1$id <- factor(Wave1$id)
Wave1$gender <- factor(Wave1$gender, levels=c(0,1),
                       labels=c("Female","Male"), ordered=F)
Wave1$idg <- factor(Wave1$idg)
Wave1$wave <- factor(Wave1$wave, ordered=F)
Wave1$round <- factor(Wave1$round)
Wave1$position <- factor(Wave1$position)
Wave1$order <- factor(Wave1$order)
Wave1$partner <- factor(Wave1$partner)
Wave1$pid <- factor(Wave1$pid)

Wave1$match <- factor(Wave1$match, levels=c(0,1),
                       labels=c("No","Yes"), ordered=T)
Wave1$samerace <- factor(Wave1$samerace, levels=c(0,1),
                          labels=c("No","Yes"), ordered=T)
Wave1$age_o <- as.numeric(Wave1$age_o)
Wave1$race_o <- factor(Wave1$race_o, levels=c(1:6), 
                        labels=c("Black/African American",
                                 "European/Caucasian-American",
                                 "Latino/Hispanic American",
                                 "Asian/Pacific Islander/Asian-American",
                                 "Native American",
                                 "Other"), ordered=F)
Wave1$dec_o <- factor(Wave1$dec_o, levels=c(0,1),
                      labels=c("No","Yes"), ordered=T)
Wave1$met_o <-factor(Wave1$met_o, levels=c(2,1),
                     labels=c("No","Yes"), ordered=T)
Wave1$age <- as.numeric(Wave1$age)
Wave1$field <- factor(Wave1$field)
Wave1$field_cd <- factor(Wave1$field_cd)
Wave1$undergra <- factor(Wave1$undergra)
Wave1$mn_sat <- as.numeric(Wave1$mn_sat)
Wave1$tuition <- as.numeric(Wave1$tuition)
Wave1$race <- factor(Wave1$race, levels=c(1:6), 
                       labels=c("Black/African American",
                                "European/Caucasian-American",
                                "Latino/Hispanic American",
                                "Asian/Pacific Islander/Asian-American",
                                "Native American",
                                "Other"), ordered=F)
Wave1$imprace <- factor(Wave1$imprace,ordered = T)
Wave1$imprelig <- factor(Wave1$imprelig,ordered = T)
Wave1$from <- factor(Wave1$from)
Wave1$zipcode <- factor(Wave1$zipcode)
Wave1$income <- as.numeric(Wave1$income)
Wave1$goal <- factor(Wave1$goal)
Wave1$date <- factor(Wave1$date)
Wave1$go_out <- factor(Wave1$go_out)
Wave1$career <- factor(Wave1$career)
Wave1$career_c <- factor(Wave1$career_c)
Wave1$sports  <- factor(Wave1$sports,ordered=T)
Wave1$tvsports  <- factor(Wave1$tvsports,ordered=T)
Wave1$exercise  <- factor(Wave1$exercise,ordered=T)
Wave1$dining  <- factor(Wave1$dining,ordered=T)
Wave1$museums <- factor(Wave1$museums,ordered=T)
Wave1$art  <- factor(Wave1$art,ordered=T)
Wave1$hiking  <- factor(Wave1$hiking,ordered=T)
Wave1$gaming  <- factor(Wave1$gaming,ordered=T)
Wave1$clubbing  <- factor(Wave1$clubbing,ordered=T)
Wave1$reading  <- factor(Wave1$reading,ordered=T)
Wave1$tv <- factor(Wave1$tv,ordered=T)
Wave1$theater  <- factor(Wave1$theater,ordered=T)
Wave1$movies  <- factor(Wave1$movies,ordered=T)
Wave1$concerts  <- factor(Wave1$concerts,ordered=T)
Wave1$music  <- factor(Wave1$music,ordered=T)
Wave1$shopping  <- factor(Wave1$shopping,ordered=T)
Wave1$yoga  <- factor(Wave1$yoga,ordered=T)
Wave1$exphappy  <- factor(Wave1$exphappy,ordered=T)
Wave1$expnum  <- factor(Wave1$expnum,ordered=T)
Wave1$dec <- factor(Wave1$dec, levels=c(0,1),
                    labels=c("No","Yes"), ordered=T)
Wave1$met <- factor(Wave1$met, levels=c(2,1),
                    labels=c("No","Yes"), ordered=T)
Wave1$satis_2 <- factor(Wave1$satis_2,ordered=T)
Wave1$you_call <- factor(Wave1$you_call, levels=c(0,1),
                    labels=c("No","Yes"), ordered=T)
Wave1$them_cal <- factor(Wave1$them_cal, levels=c(0,1),
                         labels=c("No","Yes"), ordered=T)
Wave1$date_3 <- factor(Wave1$date_3, levels=c(0,1),
                         labels=c("No","Yes"), ordered=T)

str(Wave1)

#dating Correlations
pairs(id~int_corr+dec+dec_o+like+them_cal,data=Wave1)

#regression attempts
fit1 <- lm(int_corr~them_cal,data=Wave1)
summary(fit1)
fit1


plot(y=Wave1$attr3_1,x=Wave1$attr3_2)




