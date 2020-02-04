#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que sea cuadrado con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")


#install.packages("dynamac")
library(urca)
library(dynamac)
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
# (4) Estimación del ARDL
#-----------------------------------	

#Se muestra cómo se pierden observaciones y esto impide a R realizar las regresiones
head(m)
head(lshift(m, 1))
head(dshift(m))

summary(lm(diff(m) ~ lshift(m, 1) + lshift(y, 1) + dshift(y) + dshift(i)))
#Ver error

# Estimación con dynardl 

res1 <- dynardl(m ~ y + i + remes, data = endog, 
                lags = list("m" = 1, "y" = 1, "i" =1, "remes"=1),
                diffs = c("m", "y", "i", "remes"), 
                ec = TRUE, simulate = FALSE)

summary(res1)
dynardl.auto.correlated(res1)

res2 <- dynardl(m ~ y + i + remes, data = endog, 
        lags = list("m" = 1, "y" = 1, "i" =1, "remes"=1),
        diffs = c("m", "y", "i", "remes"), 
        lagdiffs = list("m" = c(1, 2)),
        ec = TRUE, simulate = FALSE)

#con lagdiffs se corrige autocorrelación

summary(res2)
dynardl.auto.correlated(res2)

#-----------------------------------	
# (5) ARDL bounds test procedure
#-----------------------------------	

length(res2$model$residuals)
coef(res2$model)
B <- coef(res2$model)
V <- vcov(res2$model)
R <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 1) #número de coeficientes
k <- sum(R)
q <- 0

fstat <- (1/k)*t(R%*%B-q)%*%solve(R%*%V%*%t(R))%*%(R%*%B-q) 
fstat


pssbounds(obs = 244, fstat = 1.58341, tstat = 3.1470, case = 3, k = 1)


pssbounds(res2)


#Con simulaciones
set.seed(020990)

res3 <- dynardl(m ~ y + i + remes, data = endog, 
                lags = list("m" = 1, "y" = 1, "i" = 1, "remes"= 1),
                 diffs = c("m", "y", "i", "remes"), 
                lagdiffs = list("m" = c(1)),
                ec = TRUE, simulate = TRUE, range=30, sims=10000,
                shockvar= "y")

dynardl.simulation.plot(res3, type = "area", response = "levels")
dynardl.simulation.plot(res3, type = "area", response = "levels.from.mean")
dynardl.simulation.plot(res3, type = "area", response = "diffs")
dynardl.simulation.plot(res3, type = "area", response = "shock.effect.decay")
dynardl.simulation.plot(res3, type = "area", response = "cumulative.diffs", axes = F)
dynardl.simulation.plot(res3, type = "area", response = "cumulative.abs.diffs")

summary(res3$model)

res3$model

res3$pssbounds

res3$simulation

res3$EC

#https://cran.r-project.org/web/packages/dynamac/vignettes/dynamac-vignette.html








