#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que cuadre con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")


#install.packages("seasonal")
#install.packages("vars",dependencies=TRUE)
#install.packages("stats")
#install.packages("urca")
#install.packages("CADFtest")
library(CADFtest)
library(seasonal)
library(vars)
library(dplyr)
library(readxl)
library(ggplot2)
library(fpp2)
library(urca)
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


#Pruebas de raíz unitaria
ERS.1 <- summary(ur.ers(m, type = "DF-GLS", model = "trend", lags=12, selectlag="AIC"))
ERS.2 <- summary(ur.ers(y, type = "DF-GLS", model = "trend"))
ERS.3 <- summary(ur.ers(remes, type = "DF-GLS", model = "trend"))
ERS.4 <- summary(ur.ers(i, type = "DF-GLS", model = "trend"))


ERS.5 <- summary(ur.ers(d.l.m, type = "DF-GLS", model = "trend"))
ERS.6 <- summary(ur.ers(d.l.y, type = "DF-GLS", model = "trend"))
ERS.7 <- summary(ur.ers(d.l.remes, type = "DF-GLS", model = "trend"))
ERS.8 <- summary(ur.ers(d.i., type = "DF-GLS", model = "trend"))


ERS.1 #fail to reject -0.9138 
ERS.2 #fail to reject -2.4212 
ERS.3 #fail to reject -1.2694
ERS.4 #fail to reject -2.2062

ERS.5 
ERS.6
ERS.7
ERS.8

#-----------------------------------	
# (2) Estimación del VAR
#-----------------------------------	

#Almacena las variables del VAR
y <- cbind(d.l.m, d.l.y, d.l.remes, d.i) #Series de tiempo estacionarias
z <- cbind(m, y, remes, i)

#Selecciona el número óptimo de rezagos
z <-na.omit(z)
VARselect(z,lag.max = 12)

#Estima la ecuación del VAR, con el criterio de selección de rezagos de AIC
var.p <- vars::VAR(z, ic = "AIC", lag.max = 12, type = "const"
            ,exogen= exog)   

#Coeficientes estimados
summary(var.p)

#Pruebas de los residuos
# a) Normalidad

normality.test(var.p)

# b) Autocorrelación
serial.test(var.p,lags.pt = 12,type="PT.asymptotic")
serial.test(var.p,lags.bg = 4,type="BG")

# c)Homocedasticidad
arch.test(Var,lags.multi = 2)

# Pronósticos
#forecast <- forecast(var.p) #Funciona sin variables exógenas
forecast <- predict(var.p, dumvar = for.exo, n.ahead = 24, ci = 0.95)


# Funciones Impulso-Respuesta (IRF)
MyIRF <- irf(var.p, n.ahead = 24, ci = .9, cumulative = TRUE)

par(mfrow=c(2,2), cex=.6, mar=c(4,4,2,1))

plotIRF(MyIRF, lwd=3, ask=FALSE, 
        vlabels=c("Real Output","Hours"), 
        slabels=c("technology shock","demand shock"))

# Descomposición de la Varianza

myFEVD <- fevd(var.p, n.ahead = 12) 

plot(myFEVD, addbars = 2, col = c("red3", "royalblue3","orange","black","purple"))

#Gráfica de pronóstico con variables endógenas
forecast(var.p) %>%
  autoplot() + xlab("Year")

