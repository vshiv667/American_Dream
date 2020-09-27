
#Vivek Ramanathan: The American Dream - A statistical Analysis

#start log file
sink(file="pgc_log.txt", split=TRUE)

#install.packages("haven")
library(haven) 

#Change working directory and load stata data set
setwd("C:/Users/vivek_000/Desktop/BD")
atlas <- read_dta("atlas.dta") 

# for 25th percentile (low income) parent groups

#PGC tract income distribution
pgc <- subset(atlas,state == 24 & county == 033 & tract == 807000)

summary(pgc$kfr_pooled_p25)
mean(pgc$kfr_pooled_p25, na.rm=TRUE)
sd(pgc$kfr_pooled_p25, na.rm=TRUE)

#income distribution of county state, nation (population weighted)
#install.packages("matrixStats")   
library("matrixStats")

#PGC county income distribution
pgcc <- subset(atlas,state == 24 & county == 033) 

summary(pgcc$kfr_pooled_p25)
weightedMean(pgcc$kfr_pooled_p25,pgcc$count_pooled, na.rm=TRUE)
weightedSd(pgcc$kfr_pooled_p25, pgcc$count_pooled, na.rm=TRUE)

#MD state income distribution
md <- subset(atlas,state == 24)

summary(md$kfr_pooled_p25)
weightedMean(md$kfr_pooled_p25, md$count_pooled, na.rm = TRUE)
weightedSd(md$kfr_pooled_p25, md$count_pooled, na.rm = TRUE)

#America income distribution
summary(atlas$kfr_pooled_p25)

#remove nulls from weights, where kfr values exist
summary(atlas$count_pooled)
library(tidyr)
atlas = atlas %>% drop_na(count_pooled)

weightedMean(atlas$kfr_pooled_p25, atlas$count_pooled, na.rm = TRUE)
weightedSd(atlas$kfr_pooled_p25, atlas$count_pooled, na.rm = TRUE)


# for 75th percentile (high income) parent groups

#PGC tract income distribution
summary(pgc$kfr_pooled_p75)
mean(pgc$kfr_pooled_p75, na.rm=TRUE)
sd(pgc$kfr_pooled_p75, na.rm=TRUE)

#PGC county income distribution
summary(pgcc$kfr_pooled_p75)
weightedMean(pgcc$kfr_pooled_p75,pgcc$count_pooled, na.rm=TRUE)
weightedSd(pgcc$kfr_pooled_p75, pgcc$count_pooled, na.rm=TRUE)

#MD state income distribution
summary(md$kfr_pooled_p75)
weightedMean(md$kfr_pooled_p75, md$count_pooled, na.rm = TRUE)
weightedSd(md$kfr_pooled_p75, md$count_pooled, na.rm = TRUE)

#America income distribution
summary(atlas$kfr_pooled_p75)
weightedMean(atlas$kfr_pooled_p75, atlas$count_pooled, na.rm = TRUE)
weightedSd(atlas$kfr_pooled_p75, atlas$count_pooled, na.rm = TRUE)

#Install and load sandwich and lmtest packages
#install.packages("sandwich")
#install.packages("lmtest")
library(sandwich)
library(lmtest)

# Install and load ggplot2 package
#install.packages("ggplot2")
library(ggplot2)

#pooled for races
#Model 1------------------------------------------------------------------
mod1 <- lm(kfr_pooled_p75~kfr_pooled_p25, data = pgcc)
summary(mod1)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod1, vcov = vcovHC(mod1, type="HC1")) 

# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(x = kfr_pooled_p25, y = kfr_pooled_p75, color = tract)) + 
  geom_smooth(aes(x = kfr_pooled_p25, y = kfr_pooled_p75), method = "lm", se = F)


#Model 2------------------------------------------------------------------
mod2 <- lm(kfr_pooled_p25~kfr_pooled_p75, data = pgcc)
summary(mod2)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod2, vcov = vcovHC(mod2, type="HC1")) 

# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(y = kfr_pooled_p25, x = kfr_pooled_p75, color = tract)) + 
  geom_smooth(aes(y = kfr_pooled_p25, x = kfr_pooled_p75), method = "lm", se = F)

#For blacks:
#Model 3------------------------------------------------------------------
mod3 <- lm(kfr_black_p75~kfr_black_p25, data = pgcc)
summary(mod3)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod3, vcov = vcovHC(mod3, type="HC1")) 

# Draw scatter plot with linear fit line 
# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(x = kfr_black_p25, y = kfr_black_p75, color = tract)) + 
  geom_smooth(aes(x = kfr_black_p25, y = kfr_black_p75), method = "lm", se = F)

#Model 4------------------------------------------------------------------
mod4 <- lm(kfr_black_p25~kfr_black_p75, data = pgcc)
summary(mod4)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod4, vcov = vcovHC(mod4, type="HC1")) 

# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(y = kfr_black_p25, x = kfr_black_p75, color = tract)) + 
  geom_smooth(aes(y = kfr_black_p25, x = kfr_black_p75), method = "lm", se = F)

#For whites:
#Model 5------------------------------------------------------------------
mod5 <- lm(kfr_white_p75~kfr_white_p25, data = pgcc)
summary(mod5)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod5, vcov = vcovHC(mod5, type="HC1")) 

# Draw scatter plot with linear fit line 
# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(x = kfr_white_p25, y = kfr_white_p75, color = tract)) + 
  geom_smooth(aes(x = kfr_white_p25, y = kfr_white_p75), method = "lm", se = F)

#Model 6------------------------------------------------------------------
mod6 <- lm(kfr_white_p25~kfr_white_p75, data = pgcc)
summary(mod6)

#Report coefficients with heteroskedasticity robust standard errors
coeftest(mod6, vcov = vcovHC(mod6, type="HC1")) 

# Draw scatter plot with linear fit line 
ggplot(data = pgcc) + 
  geom_point(aes(y = kfr_white_p25, x = kfr_white_p75, color = tract)) + 
  geom_smooth(aes(y = kfr_white_p25, x = kfr_white_p75), method = "lm", se = F)

#model to find factors that affect kfr_pooled_p25 ?

class(pgcc)

pgcc = as.data.frame(pgcc)
class(pgcc)

cor(pgcc[, 6:64]) #too big to visualize, lets pick some covariates and compute confident interval

res1 <- cor.test(pgcc$singleparent_share1990, pgcc$kfr_pooled_p25, method = "pearson")
res1

res2 <- cor.test(pgcc$jobs_highpay_5mi_2015, pgcc$kfr_pooled_p25, method = "pearson")
res2

res3 <- cor.test(pgcc$rent_twobed2015, pgcc$kfr_pooled_p25, method = "pearson")
res3

#H0: (rho) 'P' = 0, No correlation
#Manually computing t = (r*sqrt(df)/sqrt(1-r^2)), where df = n-1-X's
t = res3$estimate * sqrt(res3$parameter)/sqrt((1-res3$estimate^2))
t
#Compute pval of two-tail test
pval = (1-pt(res3$statistic,res3$parameter))*2
pval

#Assuming Null hypothesis to be true, rho (P)=0, the probability of getting such a value 't' is just 7.842384e-06.
#That is ~99.9% of the time I get a value that is different from this.
#As p < alpha,the value is significant at 5% level of significance. Thus, we fail to accept the null hypothesis. 
#Hence, there is sufficient statistical evidence to state that the Average Rent for a Two-Bedroom Apartment 
#is correlated with Household income ($) at age 31-37 for children with parents at the 25th percentile of the national income distribution

#Alternatively the p-value based on the t-test: Beta 'B' = 0, yield the same p values
regmod = lm (pgcc$kfr_pooled_p25~pgcc$rent_twobed2015)
regmod
summary(regmod)

#t2 = b-B/s/sqrt(n) , for n>30 - t-tail is essentially a normal distribution
t2 = 6.566/1.415
t2
df=142

pval3 = (1-pt(t2,df))*2
pval3

pval2 = (1-pnorm(6.566,0,1.415))*2
pval2

#or
regmod2 = lm (pgcc$rent_twobed2015~pgcc$kfr_pooled_p25)
regmod2
summary(regmod2)



