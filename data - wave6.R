rm(list=ls())

setwd("C:/Users/dougr/Contacts/Desktop/Analytics Communication and Visualization/Assignments/Midterm Project")
library(dplyr)
library(readxl)
library(car)
library(tidyr)
library(purrr)

Wave6a <- filter(read.csv("Dating.csv"),read.csv("Dating.csv")$wave==6)
Wave6 <- Wave6a[,colSums(is.na(Wave6a))<nrow(Wave6a)] #remove all columns where all rows are NA ie. attr4_1...)

summary(Wave6)
str(Wave6)

Wave6$iid <- factor(Wave6$iid)
Wave6$id <- factor(Wave6$id)
Wave6$gender <- factor(Wave6$gender, levels=c(0,1),
                       labels=c("Female","Male"), ordered=F)
Wave6$idg <- factor(Wave6$idg)
Wave6$wave <- factor(Wave6$wave, ordered=F)
Wave6$round <- factor(Wave6$round)
Wave6$position <- factor(Wave6$position)
Wave6$order <- factor(Wave6$order)
Wave6$partner <- factor(Wave6$partner)
Wave6$pid <- factor(Wave6$pid)

Wave6$match <- factor(Wave6$match, levels=c(0,1),
                       labels=c("No","Yes"), ordered=T)
Wave6$samerace <- factor(Wave6$samerace, levels=c(0,1),
                          labels=c("No","Yes"), ordered=T)
Wave6$age_o <- as.numeric(Wave6$age_o)
Wave6$race_o <- factor(Wave6$race_o, levels=c(1:6), 
                        labels=c("Black/African American",
                                 "European/Caucasian-American",
                                 "Latino/Hispanic American",
                                 "Asian/Pacific Islander/Asian-American",
                                 "Native American",
                                 "Other"), ordered=F)
Wave6$dec_o <- factor(Wave6$dec_o, levels=c(0,1),
                      labels=c("No","Yes"), ordered=T)
Wave6$met_o <-factor(Wave6$met_o, levels=c(2,1),
                     labels=c("No","Yes"), ordered=T)
Wave6$age <- as.numeric(Wave6$age)
Wave6$field <- factor(Wave6$field)
Wave6$field_cd <- factor(Wave6$field_cd)
Wave6$undergra <- factor(Wave6$undergra)
Wave6$mn_sat <- as.numeric(Wave6$mn_sat)
Wave6$tuition <- as.numeric(Wave6$tuition)
Wave6$race <- factor(Wave6$race, levels=c(1:6), 
                       labels=c("Black/African American",
                                "European/Caucasian-American",
                                "Latino/Hispanic American",
                                "Asian/Pacific Islander/Asian-American",
                                "Native American",
                                "Other"), ordered=F)
Wave6$imprace <- factor(Wave6$imprace,ordered = T)
Wave6$imprelig <- factor(Wave6$imprelig,ordered = T)
Wave6$from <- factor(Wave6$from)
Wave6$zipcode <- factor(Wave6$zipcode)
Wave6$income <- as.numeric(Wave6$income)
Wave6$goal <- factor(Wave6$goal)
Wave6$date <- factor(Wave6$date)
Wave6$go_out <- factor(Wave6$go_out)
Wave6$career <- factor(Wave6$career)
Wave6$career_c <- factor(Wave6$career_c)
Wave6$sports  <- factor(Wave6$sports,ordered=T)
Wave6$tvsports  <- factor(Wave6$tvsports,ordered=T)
Wave6$exercise  <- factor(Wave6$exercise,ordered=T)
Wave6$dining  <- factor(Wave6$dining,ordered=T)
Wave6$museums <- factor(Wave6$museums,ordered=T)
Wave6$art  <- factor(Wave6$art,ordered=T)
Wave6$hiking  <- factor(Wave6$hiking,ordered=T)
Wave6$gaming  <- factor(Wave6$gaming,ordered=T)
Wave6$clubbing  <- factor(Wave6$clubbing,ordered=T)
Wave6$reading  <- factor(Wave6$reading,ordered=T)
Wave6$tv <- factor(Wave6$tv,ordered=T)
Wave6$theater  <- factor(Wave6$theater,ordered=T)
Wave6$movies  <- factor(Wave6$movies,ordered=T)
Wave6$concerts  <- factor(Wave6$concerts,ordered=T)
Wave6$music  <- factor(Wave6$music,ordered=T)
Wave6$shopping  <- factor(Wave6$shopping,ordered=T)
Wave6$yoga  <- factor(Wave6$yoga,ordered=T)
Wave6$exphappy  <- factor(Wave6$exphappy,ordered=T)
#Wave6$expnum  <- factor(Wave6$expnum,ordered=T)
Wave6$dec <- factor(Wave6$dec, levels=c(0,1),
                    labels=c("No","Yes"), ordered=T)
Wave6$met <- factor(Wave6$met, levels=c(2,1),
                    labels=c("No","Yes"), ordered=T)
Wave6$satis_2 <- factor(Wave6$satis_2,ordered=T)
Wave6$you_call <- factor(Wave6$you_call, levels=c(0,1),
                    labels=c("No","Yes"), ordered=T)
Wave6$them_cal <- factor(Wave6$them_cal, levels=c(0,1),
                         labels=c("No","Yes"), ordered=T)
#Wave6$date_3 <- factor(vWave6$date_3, levels=c(0,1),
#                        labels=c("No","Yes"), ordered=T)
Wave6$attr_o <- factor(Wave6$attr_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$sinc_o <- factor(Wave6$sinc_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$intel_o <- factor(Wave6$intel_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$fun_o <- factor(Wave6$fun_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$amb_o <- factor(Wave6$amb_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$shar_o <- factor(Wave6$shar_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$like_o <- factor(Wave6$like_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$prob_o <- factor(Wave6$prob_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$met_o <- factor(Wave6$met_o, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$attr <- factor(Wave6$attr, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$sinc <- factor(Wave6$sinc, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$intel <- factor(Wave6$intel, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$fun <- factor(Wave6$fun, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$amb <- factor(Wave6$amb, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$shar <- factor(Wave6$shar, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$like <- factor(Wave6$like, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$prob <- factor(Wave6$prob, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)
Wave6$met <- factor(Wave6$met, levels=c(1,2,3,4,5,6,7,8,9,10),ordered=T)

summary(Wave6)
str(Wave6)

#dating Correlations
pairs(id~int_corr+dec+dec_o+like+them_cal,data=Wave6)
pairs(int_corr~dec+age_o+age+race_o+match,data=Wave6)
pairs(int_corr~imprace+imprelig+income+goal,data=Wave6)



#regression attempts
fit1 <- lm(int_corr~pid,data=Wave6)
summary(fit1)
fit1


plot(y=Wave6$int_corr,x=Wave6$pid)




