#Tomado de: https://www.youtube.com/watch?v=rrRrI9gElYA
#########################Ordinal regression

#install.packages("ordinal")
#install.packages("rcompanion")
#install.packages("MASS")
#install.packages("brant")

library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds


options(scipen = 999) 

######################
archivo = paste0(getwd(), "/", "Ordinal.csv")
cat (archivo)

Ordinaldf <- read.csv(archivo) #read in data
View(Ordinaldf)#view data
attach(Ordinaldf) #attach file

#label nominal data

Ordinaldf$Coldplay_cat<- factor(Ordinaldf$Coldplay_cat,
                          levels = c(0,1,2,3),
                          labels = c("The WORST", "Bad", "Okay", "Pay to see them"))  #label ordinal var


Ordinaldf$Sex<- factor(Ordinaldf$Sex,
                       levels = c(1,2),
                       labels = c("male", "female"))  #label sex

#####################null model
modelnull <- clm(as.factor(Coldplay_cat)~1,
                 data = Ordinaldf,
                 link = "logit")
##########
model1 <- clm(as.factor(Coldplay_cat)~Age+ as.factor(Sex)+Stupidity,
              data = Ordinaldf,
              link = "logit")

anova(modelnull, model1)
nagelkerke(fit  = model1,
           null = modelnull)  

summary(model1)
confint(model1)
exp(coef(model1))
exp(confint(model1))

###MASS

modelt <- polr(as.factor(Coldplay_cat)~Age+ as.factor(Sex)+Stupidity,   ##have to fit the model with a different function
               data = Ordinaldf,
               Hess=TRUE)

brant(modelt)

summary(modelt)

install.packages("AER")
library(AER)
coeftest(modelt)


