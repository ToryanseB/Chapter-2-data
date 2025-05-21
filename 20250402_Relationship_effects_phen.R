########################################################################
### Modeling relationship between effect sizes and phenological data ###
########################################################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025")

#load libraries
set.seed(123)
#load libraries
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lme4)
library(MASS)#for negative binomial


#load data for Geranium 100m
phen_cooccur<-read.csv("phen_cooccur.csv")

View(phen_cooccur)



# All species -------------------------------------------------------------

#try modelling the relationship using lm. You have categorical data because of flowering overlap
mod1<- lm(effects~flowering_overlap, data=phen_cooccur)
summary(mod1)#quick summary of the model

#plot to see what the relationship looks like
ggplot(phen_cooccur,aes(flowering_overlap, effects))+geom_point()+geom_smooth(method='lm',se=TRUE,formula=y~x,col='orange')+
  theme(plot.margin = unit(c(0.1,0.1,0,0.1),'cm'))

#let's do some residual checks
plot(mod1, which=1)#should appear with no pattern, shotgun blast
plot(mod1, which=2)#should roughly fit the 1:1 line, which it does not. it looks like an S

#try modelling using glm

mod2<-glm.nb(effects ~ flowering_overlap, data = phen_cooccur)


#negative binomial
rnbinom(n,mu,size)#n is number of observations. mu is alternative parameter via mean
?rnbinom()

#poisson
rpois(n,lambda)

# Just Geranium cooccurrence ----------------------------------------------


geranium_cooccur<-phen_cooccur %>% 
  filter(sp1_name=="Geranium viscosissimum"| sp2_name == "Geranium viscosissimum")
View(geranium_cooccur)

gmod1<-lm(effects~flowering_overlap, data=geranium_cooccur)
gmod2<-lm(effects~
