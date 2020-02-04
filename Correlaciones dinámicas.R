#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que cuadre con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")


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
x <- ur.df(m, lags=12, selectlag="AIC")
x$coefficients
x$lags
x$max.lag.y
x <- CADFtest(m, max.lag.y = 12, criterion = "AIC")  
x$coefficients

# Correlaciones dinámicas

ccf(endog$m, endog$y, lag.max = 12) #Lag 0 es la correlación contemporánea; Lag=1 a 12 es con M,Y(-i); Lag=-1 a -12 es con M,Y(+i)
ccvalues=ccf(endog$m, endog$y, lag.max = 12)

