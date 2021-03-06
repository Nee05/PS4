rm(list = ls())

library(foreign)
library(stargazer)
library(haven)
library(rlang)
library(tidyverse)
library(finalfit)
library(rdrobust)
library(ggplot2)
library(binsreg)
library(rddtools)

data = read_dta("data_ps4.dta")

##Data screening

data$age_month = data$age*12
summary(data)

#Let's observe how NAs match inside the dataset
table(is.na(data$lwage1), is.na(data$nonemp)) #All nonemp NAs are also lwage1 NAs
table(is.na(data$lwage1), is.na(data$lwage0)) #Not all lwage0 NAs are also lwage1 NAs

#Observing if lwage1 NAs (the most numerous) are distributed uniformely across the dataset

Diff_mean=mean(data[is.na(data$lwage1),]$lwage0, na.rm=T)-mean(data[!is.na(data$lwage1),]$lwage0, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$lwage0, na.rm=T)+var(data[!is.na(data$lwage1),]$lwage0, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))
#Mean of lwage0 in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

Diff_mean=mean(data[is.na(data$lwage1),]$age_month, na.rm=T)-mean(data[!is.na(data$lwage1),]$age_month, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$age_month, na.rm=T)+var(data[!is.na(data$lwage1),]$age_month, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))

#Distribution of age_month in the sample of population where we do not observe the lwage1 is not statistically different
#from the sample of the population where we observe it

Diff_mean=mean(data[is.na(data$lwage1),]$nonemp, na.rm=T)-mean(data[!is.na(data$lwage1),]$nonemp, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$nonemp, na.rm=T)+var(data[!is.na(data$lwage1),]$nonemp, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))

#Distribution of nonemp in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

Diff_mean=mean(data[is.na(data$lwage1),]$jobfind, na.rm=T)-mean(data[!is.na(data$lwage1),]$jobfind, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$jobfind, na.rm=T)+var(data[!is.na(data$lwage1),]$jobfind, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))

#Distribution of jobfind in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

#We can drop out NAs of lwage1 without excessive concerns

data = subset(data, !is.na(data$lwage1))
summary(data)

##RDD diagnostics

Density <- ggplot(data, aes(x = age_month)) +
  geom_point()+
  geom_density() + 
  geom_vline(aes(xintercept = 480), 
             color = "red", linetype = "dashed", size = 1)+
  labs(x="Age (months)", y="Density", title="Distribution of age")+
  theme_classic()
Density

Prev_wage = rdplot(data$lwage0, data$age_month, c=480, p=2, nbins=60, col.dots = "black",
                   x.label = "Age (months)", y.label="Mean of previous wage (log)", title = "" )

##RDD estimates graph

unemp_RDD <- rdplot(data$nonemp, data$age_month, c=480, p=2, nbins=60, col.dots = "black",
                    x.label = "Age (months)", y.label="Unemployment duration", title = "" )

lwage1_RDD <- rdplot(data$lwage1, data$age_month, c=480, p=2, nbins=60, col.dots = "black",
                     x.label = "Age (months)", y.label="Mean of new wage (log)", title = "" )

jobfind_RDD <- rdplot(data$jobfind, data$age_month, c=480, p=2, nbins=60, col.dots = "black",
                      x.label = "Age (months)", y.label="Probability of finding a new job", title = "" )

##Regression Table

data$cutoff = ifelse(data$age_month>=480, 1, 0)

reg1A<-lm(lwage1~cutoff, data = data)
reg2A<-lm(lwage1~cutoff, data = data, subset = data$age>=35&data$age<=45)
reg3A<-lm(lwage1~cutoff*age, data = data)
reg4A<-lm(lwage1~cutoff*age+cutoff*I(age^2)+cutoff*I(age^3)+cutoff*I(age^4), data = data)
reg5A<-rdrobust(data$lwage1, data$age, c=40, covs=data$lwage0)

reg1B<-lm(jobfind~cutoff, data = data)
reg2B<-lm(jobfind~cutoff, data = data, subset = data$age>=35&data$age<=45)
reg3B<-lm(jobfind~cutoff*age, data = data)
reg4B<-lm(jobfind~cutoff*age+cutoff*I(age^2)+cutoff*I(age^3)+cutoff*I(age^4), data = data)
reg5B<-rdrobust(data$jobfind, data$age, c=40, covs=data$lwage0)

reg1C<-lm(nonemp~cutoff, data = data)
reg2C<-lm(nonemp~cutoff, data = data, subset = data$age>=35&data$age<=45)
reg3C<-lm(nonemp~cutoff*age, data = data)
reg4C<-lm(nonemp~cutoff*age+cutoff*I(age^2)+cutoff*I(age^3)+cutoff*I(age^4), data = data)
reg5C<-rdrobust(data$nonemp, data$age, c=40, covs=data$lwage0)

stargazer(reg1A, reg2A, reg3A, reg4A)
stargazer(reg1B, reg2B, reg3B, reg4B)
stargazer(reg1C, reg2C, reg3C, reg4C)

