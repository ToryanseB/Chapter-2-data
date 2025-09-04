#################################
### GAM Approach to Modelling ###
#################################

#set working directory
setwd("~/R/R_UCThesis_Rare_Plants_Alberta/Site download Feb 2025")
#load libraries
{library(tidyverse)
library(mgcv)
library(gratia)}

#load data
cooccur_traits_ger<-read.csv("cooccur_traits_ger.csv")
#filter for only those columns you need for modelling
model_data <- cooccur_traits_ger %>%
  filter(abundance_sp1 != "unknown", abundance_sp2 != "unknown") %>% #remove those species who's abundances are unknown
  mutate(f_abundance_combination = as.factor(abundance_combination)) %>% #transform abundance combo to a factor
  dplyr::select(effects, quality, flowering_overlap, flowering_synchrony, standardized_shared_visitors, f_abundance_combination) %>%
  drop_na()


# Model -------------------------------------------------------------------
  #try a smoother on both flowering overlap and shared visitors
M1<- gam(effects ~ s(flowering_overlap, fx = FALSE, k=-1, bs = "cr") + #the amount of smoothing is not  fixed to a preset value; hence, cross-validation is used to estimate the optimal amount of smoothing. cubic regression spline should be used
                    s(standardized_shared_visitors, fx = FALSE, k=-1, bs = "cr") + 
                       f_abundance_combination,
                     data = model_data,
                     method = "REML")  

summary(M1)
#deviance explained is 3.79%
#the smoother for flowering overlap is not significant
#the smoother for standardized shared visitors is significant at the 2.5% level
anova(M1)
AIC(M1)#-33131.47
gam.check(M1)

#Remove the smoother on flowering overlap because there is no need for it
M2<-gam(effects ~ flowering_overlap+f_abundance_combination +
          s(standardized_shared_visitors, fx = FALSE, k=-1, bs = "cr"),
        data = model_data,
        method = "REML")
summary(M2)
#deviance explained is 3.6%
anova(M2)#flowering overlap not significant. abundance is significant. standardized is significant. 

#plot the smoother term
plot(M2)#shows you that that are a few plants that have more shared visitors than expected, but that it increases with a exponential relationship, which is something that can biologically make sense
AIC(M2)#-33127.21, worse
gam.check(M2)#some non-normality and heteroscedasticity. Ill try an interaction term between abundance and flowering overlap

#lets try modelling the interaction assuming it is going to be linear
M3<-gam(effects ~ flowering_overlap*f_abundance_combination +
        s(standardized_shared_visitors, fx = FALSE, k=-1, bs = "cr"),
        data = model_data,
        method = "REML")
summary(M3)
#deviance explained is 3.87%

#smother for shared visitors is significant at 2.576% level
#so the f_abundance_combination on its own is not significant
#but the interaction between flowering overlap and f_abundance combination is significant for some. and flowering_overlapo is not significant.
anova(M3)
AIC(M3)#-33137.46 better
gam.check(M3)#still heteroscedasticity

#now lets try a model that thinks the relationship is NOT linear
M4 <- gam(effects ~ f_abundance_combination +
            s(flowering_overlap, by = f_abundance_combination, bs = "cr") +
            s(standardized_shared_visitors, bs = "cr", fx = FALSE, k = -1),
          data = model_data,
          method = "REML")
summary(M4)#explains 4.33%
AIC(M4)#-33152.64 better
gam.check(M4)

#now lets add an interaction between standardize shared visitors and abundance combination
M5<- gam(effects ~ f_abundance_combination +
            s(flowering_overlap, by = f_abundance_combination, bs = "cr") +
            s(standardized_shared_visitors, by = f_abundance_combination, bs = "cr"),
          data = model_data,
          method = "REML")
summary(M5)
AIC(M5)#-33152.34 not better than M4
gam.check(M5)

#try to account for heteroscedasticity using weighted variance
# Estimate group-wise residual variances from a prior model or exploratory step:
group_var <- tapply(model_data$effects, model_data$f_abundance_combination, var)

# Assign weights: inverse of variance
model_data$weights_var <- 1 / group_var[model_data$f_abundance_combination]

# Fit GAM with weights
M6 <- gam(effects ~ f_abundance_combination +
            s(flowering_overlap, by = f_abundance_combination, bs = "cr") +
            s(standardized_shared_visitors, by = f_abundance_combination, bs = "cr"),
          data = model_data,
          weights = weights_var,
          method = "REML")
gam.check(M6)#better, but still not ideal
summary(M6)
AIC(M6)#-33308.94 much better


AIC(M1,M2,M3,M4,M5,M6)#M6 has the lowest AIC

# Plot relationship -------------------------------------------------------

# Plot all smoothers (nice faceted ggplot)
draw(M6)

