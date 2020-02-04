#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que sea cuadrado con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")



#install.packages("gets")
#install.packages("lgarch")
library(lgarch)
library(urca)
library(gets)
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
# (4) Estimación de General to Specific Pruebas
#-----------------------------------	

(mod05)

mod01 <-arx(y, ar=1)
summary(mod01)


data("infldata", package = "gets")

infldata

infldata <- zooreg(infldata[, -1], frequency = 4, start = c(1989, 1))


inflMod01 <- arx(inflData[, "infl"], mc=TRUE, ar = 1:4,
                 + mxreg=inflData[, 2:4], vcov.type = "white")



inflMod02 <- arx(inflData[, "infl"], mc = TRUE, ar = 1:4,
                 + mxreg = inflData[, 2:4], arch = 1:4, vxreg = inflData[, 2:4],
                 + vcov.type = "white")




set.seed(123)
y <- arima.sim(list(ar = 0.4), 100)
eps <- lgarchSim(100, arch = 0.3, garch = 0)
plot(mod01)
yy <- arima.sim(list(ar = 0.4), 100, innov = eps)
mod01 <- arx(y, ar = 1)


mX <- matrix(rnorm(100 * 5), 100, 5) #generate a set of 5 regressors

mod02 <- arx(y, mc = TRUE, ar = 1:2, mxreg = mX, vcov.type = "white")

mod03 <- arx(eps, arch = 1)

mod04 <- arx(eps, arch = 1:3, asym = 2, vxreg = log(mX^2))

mod05 <- arx(yy, mc = TRUE, ar = 1:2, mxreg = mX, arch = 1:3, asym = 2, 
             + vxreg =log(mX^2), vcov.type = "white")


mod05 <- arx(yy, mc = TRUE, ar = 1:2, mxreg = mX, arch = 1:3, asym = 2, vxreg =log(mX^2), vcov.type = "white")


getsm05 <- getsm(mod05)

#-----------------------------------	
# (5) Estimación de General to Specific Datos Billetes y Monedas
#-----------------------------------	


inflMod04 <- arx(matrix.ts[, 1], mc = TRUE, ar = 1:12, vcov.type=c("ordinary", "white", "newey-west"), mxreg =matrix.ts[, 3:4])


inflMod05 <- getsm(inflMod04, ar.LjungB = list(lag = 12, pval = 0.025))

plot(inflMod05)
#https://www.rdocumentation.org/packages/gets/versions/0.21/topics/arx


