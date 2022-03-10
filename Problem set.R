rm(list = ls())

library(foreign)
library(stargazer)
library(haven)
library(tidyverse)
library(finalfit)

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
#Mean of age_month in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

Diff_mean=mean(data[is.na(data$lwage1),]$nonemp, na.rm=T)-mean(data[!is.na(data$lwage1),]$nonemp, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$nonemp, na.rm=T)+var(data[!is.na(data$lwage1),]$nonemp, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))
#Mean of nonemp in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

Diff_mean=mean(data[is.na(data$lwage1),]$jobfind, na.rm=T)-mean(data[!is.na(data$lwage1),]$jobfind, na.rm=T)
Var=sqrt(var(data[is.na(data$lwage1),]$jobfind, na.rm=T)+var(data[!is.na(data$lwage1),]$jobfind, na.rm=T))
2*(1-pnorm(abs(Diff_mean/Var)))
#Mean of jobfind in the part of population where we do not observe the lwage1 is not statistically different
#from the part of the population where we observe it

#We can drop NAs of lwage1 without problems

data = subset(data, !is.na(data$lwage1))
summary(data)
