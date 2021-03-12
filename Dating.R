------------------------------------------
 
 
rm(list=ls())
 
setwd("C:/Users/dougr/Contacts/Desktop/Analytics Communication and Visualization/Assignments/Midterm Project")
library(dplyr)
library(readxl)
library(car)
library(tidyr)
library(jtools)
library(MuMIn)
library(Hmisc)
 
#Wave 7  
Wave <- filter(read.csv("Dating.csv"),read.csv("Dating.csv")$wave==7)
#WaveReduced <- Wave7a[,colSums(is.na(Wave))<nrow(Wave)] #remove all columns where all rows are NA ie. attr4_1...)
 
summary(Wave)
str(Wave)
 
Wave$dec<-factor(Wave$dec, levels=c(1,0), labels=c("Yes","No"),
                       ordered=F)
Wave$dec_o<-factor(Wave$dec_o, levels=c(1,0), labels=c("Yes","No"),
                         ordered=F)
 
Wave<-mutate(.data=Wave, DateNum=case_when(dec=="Yes" ~ 1,
                                                 dec=="No" ~ 0))
Wave<-mutate(.data=Wave, DateNum_o=case_when(dec_o=="Yes" ~ 1,
                                                 dec_o=="No" ~ 0))
 
# Impute the intel_o mean on missing intel_o
Wave%>%mutate(intel_o=case_when(is.na(Wave$intel_o)~mean(Wave$intel_o,na.rm = TRUE),
                                 TRUE~Wave$intel_o))->Wave
Wave%>%mutate(attr_o=case_when(is.na(Wave$attr_o)~mean(Wave$attr_o,na.rm = TRUE),
                                TRUE~Wave$attr_o))->Wave
Wave%>%mutate(sinc_o=case_when(is.na(Wave$sinc_o)~mean(Wave$sinc_o,na.rm = TRUE),
                                TRUE~Wave$sinc_o))->Wave
Wave%>%mutate(amb_o=case_when(is.na(Wave$amb_o)~mean(Wave$amb_o,na.rm = TRUE),
                               TRUE~Wave$amb_o))->Wave
Wave%>%mutate(like_o=case_when(is.na(Wave$like_o)~mean(Wave$like_o,na.rm = TRUE),
                               TRUE~Wave$like_o))->Wave
Wave%>%mutate(shar_o=case_when(is.na(Wave$shar_o)~mean(Wave$shar_o,na.rm = TRUE),
                               TRUE~Wave$shar_o))->Wave
Wave%>%mutate(prob_o=case_when(is.na(Wave$prob_o)~mean(Wave$prob_o,na.rm = TRUE),
                               TRUE~Wave$prob_o))->Wave
Wave%>%mutate(pf_o_fun=case_when(is.na(Wave$pf_o_fun)~mean(Wave$pf_o_fun,na.rm = TRUE),
                               TRUE~Wave$pf_o_fun))->Wave
 
summary(Wave)
str(Wave)
 
table(Wave$dec)
table(Wave$dec_o)
summary(Wave)
str(Wave)
 
#dating Correlations
pairs(dec_o~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o+like_o+prob_o+pf_o_fun,data=Wave)
pairs(int_corr~attr_o+sinc_o+intel_o+fun_o+amb_o+shar_o+like_o+prob_o+pf_o_fun,data=Wave)
 
#linear regression - using dec_o
fit2d <- lm(dec_o~attr_o+intel_o+like_o+prob_o,data=Wave) #best
summary(fit2d)
 
vif(fit2d)
AICc(fit2d)
 
#GLM, dec_o~attr_o+sinc_o+amb_o+like_o+shar_o+prob_o+pf_o_fun
fit2glm <- glm(data=Wave,dec_o~attr_o+intel_o+like_o+prob_o,family ="binomial")
summary(fit2glm)
 
vif(fit2glm)
AIC(fit2glm)
 
#confusion Matrix
Wave<-mutate(.data=Wave, Prediction=predict(fit2glm,type="response"))
Wave<-mutate(.data=Wave, KindPred=case_when(Prediction>0.5~"Yes",
                                                        TRUE~"No"))
 
confusion<-table(Wave$dec_o, Wave$KindPred,dnn=c("Actual","Predicted"))
confusion
 
print(paste("Fraction of Correct Predictions:",
            round((confusion["CH","CH"]+confusion["MM","MM"])/sum(confusion),2)))
print(paste("The Overall Error Rate:",
            round((confusion["CH","MM"]+confusion["MM","CH"])/sum(confusion),2)))
 
 
plot(y=Wave$dec_o,x=Wave$attr_o)
plot(y=Wave$dec_o,x=Wave$sinc_o)
plot(y=Wave$dec_o,x=Wave$amb_o)
plot(y=Wave$dec_o,x=Wave$like_o)
plot(y=Wave$dec_o,x=Wave$prob_o)
plot(y=Wave$dec_o,x=Wave$pf_o_fun)
Doug Andrade
