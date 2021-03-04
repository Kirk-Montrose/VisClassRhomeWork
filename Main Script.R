setwd("/Users/kirkmontrose/Desktop/") #/Change this to point at you own location 
dataFromDating <- read.csv("dating.csv")


fit <- lm(wave~id , data = dataFromDating )


print(summary(fit))
