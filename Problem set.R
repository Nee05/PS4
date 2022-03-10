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

remotes::install_github('bquast/rddtools')
install.packages("rddtools")
vignette('rddtools')
View(data)
#trial <- rdd_data(y=rdd_data$age, x=rdd_data$, cutpoint=0)
A <- ggplot(data, aes(x = age)) +
  geom_density()
A
A + geom_vline(aes(xintercept = mean(age)), 
               color = "red", linetype = "dashed", size = 1)
data$binedage<-cut(data$age_month, seq(min(data$age_month),max(data$age_month), 4))
View(data)
B<- ggplot(data, aes(x=binedage,
                     y = lwage0)) +
  geom

#without bin size B
B<- ggplot(data, aes(x =age,
                     y = lwage0)) +
  geom_point(alpha = 0.4) +
  stat_summary_bin(fun.y = "mean" , bins = 20,
                   color = "yellow", size = 2, geom = "point")
B
B + geom_vline(aes(xintercept = mean(age)), 
               color = "red", linetype = "dashed", size = 1)

data$age_month = data$age*12

install.packages("binsreg")
library(binsreg)

#B plot 2
binscatter1<-binsreg(rdd_data$lwage0, rdd_data$age, dotsgrid = 4, polyreg=2)
binscatter1$bins_plot + geom_vline (xintercept = 39, colour = "red" ) + xlab("Age") + ylab("Log of wages at layoff")
ggtitle("Plot of age vs wages at layoff") + 
  theme(plot.title = element_text(hjust = 0.3, vjust = 0.3, face = 'italic')) +
  theme(element_line(margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")))

install.packages("rdrobust")
library(rdrobust)
a<- rdplot(data$binedage, data$lwage0, nbins = data$binedage, binselect = "espr")
summary(a)


