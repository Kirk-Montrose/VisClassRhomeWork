rm(list=ls())

setwd("C:/Users/dougr/Contacts/Desktop/Analytics Communication and Visualization/Assignments/Midterm Project")
library(dplyr)
library(readxl)
library(car)
library(tidyr)
library(jtools)
library(MuMIn)
library(Hmisc)

Wave7a <- filter(read.csv("Dating.csv"),read.csv("Dating.csv")$wave==7)
Wave7 <- Wave7a[,colSums(is.na(Wave7a))<nrow(Wave7a)] #remove all columns where all rows are NA ie. attr4_1...)

summary(Wave7)
str(Wave7)

Wave7$dec<-factor(Wave7$dec, levels=c(1,0), labels=c("Yes","No"),
                       ordered=F)
Wave7$dec_o<-factor(Wave7$dec_o, levels=c(1,0), labels=c("Yes","No"),
                         ordered=F)

Wave7<-mutate(.data=Wave7, DateNum=case_when(dec=="Yes" ~ 1,
                                                 dec=="No" ~ 0))
Wave7<-mutate(.data=Wave7, DateNum_o=case_when(dec_o=="Yes" ~ 1,
                                                 dec_o=="No" ~ 0))

summary(Wave7)
str(Wave7)

table(Wave7$dec)
table(Wave7$dec_o)
summary(Wave7)
str(Wave7)

#dating Correlations
pairs(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o+like_o+prob_o+pf_o_fun,data=Wave7)
pairs(int_corr~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o+like_o+prob_o+pf_o_fun,data=Wave7)

#linear regression - using dec_o (best), int_corr, like_o
fit2d <- lm(dec_o~attr_o+sinc_o+amb_o+like_o+shar_o+prob_o+pf_o_fun,data=Wave7) #best
summary(fit2d)

vif(fit2d)
AICc(fit2d)

#GLM, dec_o~attr_o+sinc_o+amb_o+like_o+shar_o+prob_o+pf_o_fun
fit2glm <- glm(data=Wave7,dec_o~attr_o+sinc_o+like_o+pf_o_fun,family ="binomial")
summary(fit2glm)

vif(fit2glm)
AIC(fit2glm)

#confusion Matrix
Wave7<-mutate(.data=Wave7, Prediction=predict(fit2glm,type="response"))
Wave7<-mutate(.data=Wave7, KindPred=case_when(Prediction>0.5~"Yes",
                                                        TRUE~"No"))

confusion<-table(Wave7$dec_o, Wave7$KindPred,dnn=c("Actual","Predicted"))
confusion

print(paste("Fraction of Correct Predictions:",
            round((confusion["CH","CH"]+confusion["MM","MM"])/sum(confusion),2)))
print(paste("The Overall Error Rate:",
            round((confusion["CH","MM"]+confusion["MM","CH"])/sum(confusion),2)))


plot(y=Wave2$dec_o,x=Wave2$attr_o)
plot(y=Wave2$dec_o,x=Wave2$sinc_o)
plot(y=Wave2$dec_o,x=Wave2$amb_o)
plot(y=Wave2$dec_o,x=Wave2$like_o)
plot(y=Wave2$dec_o,x=Wave2$prob_o)
plot(y=Wave2$dec_o,x=Wave2$pf_o_fun)
