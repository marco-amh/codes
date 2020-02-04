#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que sea cuadrado con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")


#install.packages("lme4", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
#install.packages("nlme", repos="http://cran.rstudio.com/",type = "binary", dependencies=TRUE)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")
#install.packages("ecm")

library(urca)
library(ecm)
library(seasonal)
library(dplyr)
library(readxl)
library(ggplot2)
library(fpp2)

#-----------------------------------	
# (1) Prepara los datos	
#-----------------------------------	

#Importa los datos con frecuencia mensual	
endog <- read_excel("Base_Mensual.xlsx",sheet = "Endógenas")
exog <- read_excel("Base_Mensual.xlsx",sheet = "Exógenas")
for.exo <- read_excel("Base_Mensual.xlsx",sheet = "Forecast_Exo")
names(endog)
names(exog)

# Se crean las series de tiempo
k <- 1995 #año de inicio
f <- 12 #frecuencia

matrix.ts <- ts(endog[1:6], frequency = f, start = k) #Matriz con los vectores de series de tiempo

# Series de tiempo
m <- ts(endog$m, start=c(k), frequency=f); #billetes y monedas
y <- ts(endog$y, start=c(k), frequency=f); #IGAE
i <- ts(endog$i, start=c(k), frequency=f); #tasa de interés
p <- ts(endog$p, start=c(k), frequency=f); #INPC
remes <- ts(endog$remes, start=c(k), frequency=f); #remesas

# Transforma las variables en logaritmos.	
l.m <- log(ts(endog$m, start=c(k), frequency=f)); #billetes y monedas
l.y <- log(ts(endog$y, start=c(k), frequency=f)); #IGAE
l.p <- log(ts(endog$p, start=c(k), frequency=f)); #INPC
l.remes <- log(ts(endog$remes, start=c(k), frequency=f)); #remesas

# Se crean las fechas 	
date <- time(m);	

# Transforma las variables en primeras diferencias
d.l.m <- diff(l.m, differences = 1)
d.l.y <- diff(l.y, differences = 1)
d.l.remes <- diff(l.remes, differences = 1)
d.i <- diff(i, differences = 1)


#-----------------------------------	
# (2) Inspección visual sobre integración de las series
#-----------------------------------	

autoplot(m) + xlab("Year") +
  ggtitle("Billetes y monedas")

autoplot(y) + xlab("Year") +
  ggtitle("IGAE")

autoplot(i) + xlab("Year") +
  ggtitle("Cetes 28 días")

autoplot(p) + xlab("Year") +
  ggtitle("INPC")

autoplot(remes) + xlab("Year") +
  ggtitle("Remesas")

#-----------------------------------	
# (3) Pruebas de raíz unitaria
#-----------------------------------	

#Niveles
#1. Dickey Fuller Aumentada: Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.df(y, type = c("none"), lags = 1))
summary(ur.df(m, type = c("none"), lags = 1))
summary(ur.df(i, type = c("none"), lags = 1))
summary(ur.df(p, type = c("none"), lags = 1))
summary(ur.df(remes, type = c("none"), lags = 1))

#2. Phillips Perron: Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.pp(m, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(y, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(i, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(p, type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(remes, type = c("Z-tau"), model = c("constant"), use.lag = 1))

#3. Elliot, Rothenberg and Stock : Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.ers(m, type = c("DF-GLS"), model = c("constant"), lag.max = 1))
summary(ur.ers(y, type = c("DF-GLS"), model = c("constant"), lag.max = 1))
summary(ur.ers(i, type = c("DF-GLS"), model = c("constant"), lag.max = 1))
summary(ur.ers(p, type = c("DF-GLS"), model = c("constant"), lag.max = 1))
summary(ur.ers(remes, type = c("DF-GLS"), model = c("constant"), lag.max = 1))

#4. KPSS : Ho: Estacionariedad. test value tiene que ser mayor a los valores críticos
summary(ur.kpss(m, type = c("mu"), use.lag = 1))
summary(ur.kpss(y, type = c("mu"), use.lag = 1))
summary(ur.kpss(i, type = c("mu"), use.lag = 1))
summary(ur.kpss(p, type = c("mu"), use.lag = 1))
summary(ur.kpss(remes, type = c("mu"), use.lag = 1))

#Diferencias
#1. Dickey Fuller Aumentada: Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.df(diff(m), type = c("none"), lags = 1))
summary(ur.df(diff(y), type = c("none"), lags = 1))
summary(ur.df(diff(i), type = c("none"), lags = 1))
summary(ur.df(diff(p), type = c("none"), lags = 1))
summary(ur.df(diff(remes), type = c("none"), lags = 1))

#2. Phillips Perron: Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.pp(diff(m), type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(diff(y), type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(diff(i), type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(diff(p), type = c("Z-tau"), model = c("constant"), use.lag = 1))
summary(ur.pp(diff(remes), type = c("Z-tau"), model = c("constant"), use.lag = 1))

#3. Elliot, Rothenberg and Stock : Ho: Unit root. test value tiene que ser mayor a los valores críticos
summary(ur.ers(diff(m), type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.ers(diff(y), type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.ers(diff(i), type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.ers(diff(p), type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 
summary(ur.ers(diff(remes), type = c("DF-GLS"), model = c("constant"), lag.max = 1)) 

#4. KPSS : Ho: Estacionariedad. test value tiene que ser mayor a los valores críticos
summary(ur.kpss(diff(m), type = c("mu"), use.lag = 1))
summary(ur.kpss(diff(y), type = c("mu"), use.lag = 1))
summary(ur.kpss(diff(i), type = c("mu"), use.lag = 1))
summary(ur.kpss(diff(p), type = c("mu"), use.lag = 1))
summary(ur.kpss(diff(remes), type = c("mu"), use.lag = 1))

#-----------------------------------	
# (4) Estimación del Error Correction Model
#-----------------------------------	

#Se muestra cómo se pierden observaciones y esto impide a R realizar las regresiones
head(m)
head(lshift(m, 1))
head(dshift(m))

summary(lm(diff(m) ~ lshift(m, 1) + lshift(y, 1) + dshift(y) + dshift(i)))
#Ver error

# Estimación con ECM package 

#y <- The target variable
#xeq <- The variables to be used in the equilibrium term of the error correction model
#xtr <-The variables to be used in the transient term of the error correction model


base <- as.data.frame(endog[c('m','y', 'i', 'remes')])

names(base)[names(base) == "y"] <- "igae"

xeq <- as.data.frame(base[c('igae', 'i', 'remes')])
xtr <- as.data.frame(base[c('igae', 'i', 'remes')])

ecm1 <- ecm(base$m, xeq, xtr, includeIntercept = TRUE)

ecm1$coefficients
ecm1$fitted.values
ecm1$residuals
ecm1$effects
summary(ecm1)

#-----------------------------------	
# (5) Loop 
#-----------------------------------	
#https://stackoverflow.com/questions/47814486/for-loop-regression-estimations-ecm-model

f <- function(.) {
  xeq <- as.data.frame(select(., igae, i, remes))
  xtr <- as.data.frame(select(., igae, i, remes))
  print(xeq)
  print(xtr)
  summary(ecm(.$m, xeq, xtr, includeIntercept = TRUE))
}


Models <- base %>% 
  group_by(Category, Brand) %>% 
  do(Model = f(.))


Models$Category
[1] "2" "3"
Models$Brand
[1] "3" "3"
Models$Model
[[1]]


