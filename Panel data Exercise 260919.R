# This script is to practice panel data
# Marco Martinez Huerta 26.9.19	

#-----------------------------------	
# (0) Clear memory and call packages	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Panel data")	

#install.packages("gplots")
#install.packages("plm")


library (foreign)
library(car) #helps make scatterplots
library(gplots)
library(plm) #panel data
library(tseries)
library(lmtest)

panel <- read.dta("https://dss.princeton.edu/training/Panel101.dta")

coplot(y ~ year | country, type="l", data=panel) #lines
coplot(y ~ year | country, type="p", data=panel) #points and lines

#scatterplot (y ~ year | country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=panel)

plotmeans(y~country, main="Heterogeneity across countries", data=panel) #draws a 95% confidence interval around the mean
plotmeans(y~year, main="Heterogeneity across years", data=panel) #draws a 95% confidence interval around the mean

ols <- lm(y ~x1, data=panel) #regular ols regression does not consider heterogeneity across groups or time
summary(ols)
yhat <- ols$fitted

#The coefficient x1 indicates how much Y changes when X increases by one unit. 
#The x1 coefficient has to be less than p<0.05

plot(panel$x1, panel$y, pch=19, xlab="x1", ylab="y")

abline(lm(panel$y ~ panel$x1), lwd=3, col="red")


#Fixed effects using Least Squares Dummy Variable (LSDV) model

fixed.dum <- lm(y ~ x1 + factor(country)-1, data=panel) # -1 removes the constant term

summary(fixed.dum)

#x1 is significant at 2.9%

#Each component of the factor variable (dummy) is absorbing the effects particular to each country 
#Predictor x1 was not significant in the OLS model, once controlling for differences across countries, x1 became significant in the OLS_DUM
#The coefficient of x1 indicates how mucho Y changes over time, controlling by differences in countries, when X increases by one unit. 


yhat <- fixed.dum$fitted

scatterplot(yhat ~ panel$x1 | panel$country, boxplots=FALSE, xlab="x1", ylab="yhat", smooth=FALSE )
abline(lm(panel$y ~ panel$x1,), lwd=3, col="red")

#Fixed effects: n entity-specific intercepts (using plm)

fixed <- plm(y ~ x1, data=panel, index=c("country", "year"), model="within") #model="within" fixed effects option; index= panel setting

summary(fixed)

#The coefficient of x1 indicates how much Y changes over time, on average per country, when X increases one unit.
#If the p-value of the F test is <0.05 the model is ok. This is a test F to see whether all the coefficients in the model are different form zero.

fixef(fixed) #displays the fixed effects (constants for each country)

pFtest(fixed,ols) #Testing for fixed effects, null: OLS better than fixed

#If the p-value is <0.05 then the fixed effects model is a better choice.


#Random-Effects model (random intercept, partial pooling model)

random <- plm(y ~ x1, data=panel, index=c("country", "year"), model="random") #model="random" effects option; index= panel setting

#Interpretation of the coefficients is tricky since they include both the within-entity and between entity effects. 
#In the case of TSCS data represents the average effect of X over Y when X changes across time and between countries by one unit

#Setting as panel data (alternative way to run the above model)

#Fixed or Random?
#To decide between fixed or random effects you can run a Hausman test
#The null hypothesis is that preferred model is random effects vs the alternative the fixed effects
#It tests whether the unique errors (ui) are correlated with the regressors, the null hypothesis is they are not
#Run a fixed effects and save the estimates, then run a random model and save the estimates, the perform the test. 
#If the p-value is <0.05, then use fixed effects

phtest(fixed,random)

#Testing for time fixed effects
fixed <- plm(y ~ x1, data=panel, index=c("country", "year"), model="within") #model="within" fixed effects option; index= panel setting
fixed.time <- plm(y ~ x1 + factor(year), data=panel, index=c("country", "year"), model="within")
summary(fixed.time)

#Testing time fixed effects. The null hypothesis is that no time fixed needed
#If the p-value is <0.05 then use time-fixed effects. 

pFtest(fixed.time,fixed)
plmtest(fixed,c("time"), type=("bp"))

#Testing for random effects: Breusch-Pagan Lagrange Mutiplier (LM)
#The LM test helps you to decide between a random effects regression and a simple OLS regression.
#The null hypothesis in the LM test is that variances across entities is zero. This is, no significant difference across units. 

#Regular OLS (pooling model) using plm
pool <- plm(y ~ x1, data=panel, index=c("country", "year"), model="pooling")
summary(pool)

#Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (OLS better)
plmtest(pool, type=c("bp"))
#If p-value is <0.05 reject the null and conclude that random effects is appropiate

#Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test of independence and Pasaran CD test

#Cross-sectional dependence is a problem in macro panels with long time series. There is no much problem in micro panels
#The null hypothesis in the BP LM and Pasaran CD tests of independence is that residuals across entities are not correlated.This test is used to test whether the residuals are correlated across countries.
#Cross sectional dependence can lead to bias in tests results (contemporaneous correlation)

fixed <- plm(y ~ x1, data=panel, index=c("country","year"), model="within")
pcdtest(fixed, test=c("lm"))

pcdtest(fixed, test=c("cd"))
#If the p-value is <0.05 there is cross sectional dependence

#Testing for serial autocorrelation
#The null hypothesis is that there is not serial correlation
pbgtest(fixed)

#If p-value is <0.05 there is serial correlation

#Testing for unit roots/statonarity

#The Dickey Fuller test to check for stochastic trends. 
#The null hypothesis is that the series has a unit root
#If unit root is present   you can take the first difference of the variable

Panel.set <- pdata.frame(panel, index=c("country","year"))

adf.test(panel.set$y, k=2)
#If p-value <0.05 then there is no unit root problem


#Testing for heteroskedasticity
#The null hypothesis for the Breusch-Pagan test is homoskedasticity

bptest(y ~ x1 + factor(country), data=panel, studentize=F)

#If p-value <0.05 there is presence of heteroskedasticity

#If hetersokedasticity is detected you can use robust covariance matrix to account for it. 

#Controlling for heteroskedasticity: Robust covariance matriz estimation (sandwich estimator)
#The "vcovHC" function estimates three heteroskedasticity-consisten covariance

# a) "white1" is for general heteroskedasticity but no serial correlation. Recommended for random effects
# b) "white2" is white1 restricted to a common variance within groups. Recommended for random effects
# c) "arellano" both heteroskedasticity and serial correlation. Recommended for fixed effects

#The following options apply:

# HC0 - heteroskedasticity consisten. The default
# HC1, HC2, HC3 - Recommended for small samples. HC3 gives less weight to influential observations
# HC4 small samples with influential observations
# HAC heteroskedasticity and autocorrelation consistent (type ?vcovHAC for details)

#Controlling for heteroskedasticity: random effects
random <- plm(y ~ x1, data=panel, index=c("country", "year"), model="random")
coeftest(random) #original coefficients
coeftest(random, vcovHC) #Heteroskedasticity consistent coefficients
coeftest(random, vcovHC(random, type="HC3")) #Heteroskedasticity consistent coefficients, type=3

#The following shows the HC standard errors of the coefficients
#t(sapply(c("HCO","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(vcovHC(random, type=x)))))



#Controlling for heteroskedasticity: fixed effects
fixed <- plm(y ~ x1, data=panel, index=c("country", "year"), model="within")
coeftest(fixed) #original coefficients
coeftest(fixed, vcovHC) #Heteroskedasticity consistent coefficients
coeftest(random, vcovHC(fixed, method="arellano")) #Heteroskedasticity consistent coefficients, "Arellano"
coeftest(random, vcovHC(fixed, type="HC3")) #Heteroskedasticity consistent coefficients, type=3


