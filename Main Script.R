library(dplyr)
library(ggplot2)
library(tidyr)
#library(logit)

setwd("~/Homework/VisClassRhomeWork/Data") #Set your on path
#setwd("~/Documents/Github/VisClassRhomeWork/Data")
#setwd("~/Documents/Github/VisClassRhomeWork/Data") #Set your on path
dataFromDating <- read.csv("dating.csv")
filterData <- filter(dataFromDating,wave ==7) #Get Data from wave 7.

summary(filterData)
str(filterData)

#factor data

filterData$dec<-factor(filterData$dec, levels=c(1,0), labels=c("Yes","No"),
                       ordered=F)
filterData$dec_o<-factor(filterData$dec_o, levels=c(1,0), labels=c("Yes","No"),
                         ordered=F)

data<-mutate(.data=filterData, DateNum=case_when(dec=="Yes" ~ 1,
                                                 dec=="No" ~ 0))
data<-mutate(.data=filterData, DateNum=case_when(dec_o=="Yes" ~ 1,
                                                 dec_o=="No" ~ 0))
summary(filterData)
str(filterData)

table(filterData$dec)
table(filterData$dec_o)

#try different fits

fit<-glm(data=filterData, dec~attr1_1,family="binomial")
summary(fit)

vif(fit)

AIC(fit)
BIC(fit)
AICc(fit)

fit1<-glm(data=filterData, dec~attr1_1+int_corr,family="binomial")
summary(fit1)

vif(fit1)

AIC(fit1)
BIC(fit1)
AICc(fit1)

fit2<-glm(data=filterData, dec~attr1_1+int_corr+exphappy,family="binomial")
summary(fit2)

vif(fit2)

AIC(fit2)
BIC(fit2)
AICc(fit2)

#confusion matrix

filterData<-mutate(.data=filterData, Prediction=predict(fit2,type="response"))
filterData<-mutate(.data=filterData, KindPred=case_when(Prediction>0.5~"Yes",
                                                        TRUE~"No"))

confusion<-table(filterData$dec, filterData$KindPred,dnn=c("Actual","Predicted"))
confusion

#plot

library(jtools)
effect_plot(fit2, pred=attr1_1)
effect_plot(fit2, pred=int_corr)
effect_plot(fit2, pred=exphappy)