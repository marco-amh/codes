#REVISAR ESTO: Hay que cambiar todo a formato TS y luego hacer window para acotar la muestra de dummies para que sea cuadrado con los rezagos.

#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//R//Pronósticos mensuales")

#install.packages("xts")
#library(xts)
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
names(endog)

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


#-----------------------------------	
# (2) Descomposición simple aditiva
#-----------------------------------	

m.sa.add <- decompose(m, type="additive")
m.sa.add.trend <- m.sa.add$trend
m.sa.add.seasonal <- m.sa.add$seasonal
m.sa.add.random <- m.sa.add$random

autoplot(m.sa.add) + xlab("Year") +
  ggtitle("Classical additive decomposition
          of billetes y monedas")

y.sa.add <- decompose(y, type="additive")
y.sa.add.trend <- y.sa.add$trend
y.sa.add.seasonal <- y.sa.add$seasonal
y.sa.add.random <- y.sa.add$random


autoplot(y.sa.add) + xlab("Year") +
  ggtitle("Classical additive decomposition
          of billetes y monedas")


p.sa.add <- decompose(p, type="additive")
p.sa.add.trend <- p.sa.add$trend
p.sa.add.seasonal <- p.sa.add$seasonal
p.sa.add.random <- p.sa.add$random


autoplot(p.sa.add) + xlab("Year") +
  ggtitle("Classical additive decomposition
          of billetes y monedas")

remes.sa.add <- decompose(remes, type="additive")
remes.sa.add.trend <- remes.sa.add$trend
remes.sa.add.seasonal <- remes.sa.add$seasonal
remes.sa.add.random <- remes.sa.add$random

autoplot(remes.sa.add) + xlab("Year") +
  ggtitle("Classical additive decomposition
          of billetes y monedas")


#-----------------------------------	
# (3) Descomposición simple multiplicativa
#-----------------------------------	

m.sa.mult <- decompose(m, type="multiplicative")
m.sa.mult.trend <- m.sa.mult$trend
m.sa.mult.seasonal <- m.sa.mult$seasonal
m.sa.mult.random <- m.sa.mult$random

autoplot(m.sa.mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of billetes y monedas")

y.sa.mult <- decompose(y, type="multiplicative")
y.sa.mult.trend <- y.sa.mult$trend
y.sa.mult.seasonal <- y.sa.mult$seasonal
y.sa.mult.random <- y.sa.mult$random

autoplot(y.sa.mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of billetes y monedas")


p.sa.mult <- decompose(p, type="multiplicative")
p.sa.mult.trend <- p.sa.mult$trend
p.sa.mult.seasonal <- p.sa.mult$seasonal
p.sa.mult.random <- p.sa.mult$random

autoplot(p.sa.mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of billetes y monedas")

remes.sa.mult <- decompose(remes, type="multiplicative")
remes.sa.mult.trend <- remes.sa.mult$trend
remes.sa.mult.seasonal <- remes.sa.mult$seasonal
remes.sa.mult.random <- remes.sa.mult$random

autoplot(remes.sa.mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of billetes y monedas")

#-----------------------------------	
# (4) X11 Decomposition
#-----------------------------------	

m.sa.x11 <- seas(m,x11="")
m.sa.x11.trend <- trendcycle(m.sa.x11)
m.sa.x11.seasonal <- seasonal(m.sa.x11)
m.sa.x11.remainder <- remainder(m.sa.x11)
m.sa.x11.ajusted <- seasadj(m.sa.x11)

m.sa.x11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the X11 decomposition 

autoplot(m.sa.x11) +
  ggtitle("X11 decomposition of billetes y monedas")


y.sa.x11 <- seas(y,x11="")
y.sa.x11.trend <- trendcycle(y.sa.x11)
y.sa.x11.seasonal <- seasonal(y.sa.x11)
y.sa.x11.remainder <- remainder(y.sa.x11)
y.sa.x11.ajusted <- seasadj(y.sa.x11)

y.sa.x11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the X11 decomposition 

autoplot(y.sa.x11) +
  ggtitle("X11 decomposition of billetes y monedas")


p.sa.x11 <- seas(p,x11="")
p.sa.x11.trend <- trendcycle(p.sa.x11)
p.sa.x11.seasonal <- seasonal(p.sa.x11)
p.sa.x11.remainder <- remainder(p.sa.x11)
p.sa.x11.ajusted <- seasadj(p.sa.x11)

p.sa.x11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the X11 decomposition 

autoplot(m.sa.x11) +
  ggtitle("X11 decomposition of billetes y monedas")

?seas
remes.sa.x11 <- seas(remes,x11="")
remes.sa.x11.trend <- trendcycle(remes.sa.x11)
remes.sa.x11.seasonal <- seasonal(remes.sa.x11)
remes.sa.x11.remainder <- remainder(remes.sa.x11)
remes.sa.x11.ajusted <- seasadj(remes.sa.x11)

remes.sa.x11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the X11 decomposition 

autoplot(m.sa.x11) +
  ggtitle("X11 decomposition of billetes y monedas")

#-----------------------------------	
# (5) TRAMO SEATS
#-----------------------------------	


m.sa.SEATS <- seas(m)
m.sa.SEATS.trend <- trendcycle(m.sa.SEATS)
m.sa.SEATS.seasonal <- seasonal(m.sa.SEATS)
m.sa.SEATS.remainder <- remainder(m.sa.SEATS)
m.sa.SEATS.ajusted <- seasadj(m.sa.SEATS)

m.sa.SEATS %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(m.sa.SEATS) +
  ggtitle("TRAMO SEATS decomposition of billetes y monedas")


y.sa.SEATS <- seas(y)
y.sa.SEATS.trend <- trendcycle(y.sa.SEATS)
y.sa.SEATS.seasonal <- seasonal(y.sa.SEATS)
y.sa.SEATS.remainder <- remainder(y.sa.SEATS)
y.sa.SEATS.ajusted <- seasadj(y.sa.SEATS)

y.sa.SEATS %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(y.sa.SEATS) +
  ggtitle("TRAMO SEATS decomposition of billetes y monedas")


p.sa.SEATS <- seas(p)
p.sa.SEATS.trend <- trendcycle(p.sa.SEATS)
p.sa.SEATS.seasonal <- seasonal(p.sa.SEATS)
p.sa.SEATS.remainder <- remainder(p.sa.SEATS)
p.sa.SEATS.ajusted <- seasadj(p.sa.SEATS)

p.sa.SEATS %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(m.sa.SEATS) +
  ggtitle("TRAMO SEATS decomposition of billetes y monedas")


remes.sa.SEATS <- seas(remes)
remes.sa.SEATS.tremd <- trendcycle(remes.sa.SEATS)
remes.sa.SEATS.seasonal <- seasonal(remes.sa.SEATS)
remes.sa.SEATS.remainder <- remainder(remes.sa.SEATS)
remes.sa.SEATS.ajusted <- seasadj(remes.sa.SEATS)

remes.sa.SEATS %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(remes.sa.SEATS) +
  ggtitle("TRAMO SEATS decomposition of billetes y monedas")


#-----------------------------------	
# (6) Seasonal and Trending Decomposition using LOESS
#-----------------------------------	

m.sa.STL <- stl(m,t.window=13, s.window="periodic", robust=TRUE)
m.sa.STL.trend <- trendcycle(m.sa.STL)
m.sa.STL.seasonal <- seasonal(m.sa.STL)
m.sa.STL.remainder <- remainder(m.sa.STL)
m.sa.STL.ajusted <- seasadj(m.sa.STL)

m.sa.STL %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(m.sa.STL) +
  ggtitle("STL decomposition of billetes y monedas")


y.sa.STL <- stl(y,t.window=13, s.window="periodic", robust=TRUE)
y.sa.STL.trend <- trendcycle(y.sa.STL)
y.sa.STL.seasonal <- seasonal(y.sa.STL)
y.sa.STL.remainder <- remainder(y.sa.STL)
y.sa.STL.ajusted <- seasadj(y.sa.STL)

y.sa.STL %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(y.sa.STL) +
  ggtitle("STL decomposition of billetes y monedas")


p.sa.STL <- stl(p,t.window=13, s.window="periodic", robust=TRUE)
p.sa.STL.trend <- trendcycle(p.sa.STL)
p.sa.STL.seasonal <- seasonal(p.sa.STL)
p.sa.STL.remainder <- remainder(p.sa.STL)
p.sa.STL.ajusted <- seasadj(p.sa.STL)

p.sa.STL %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(p.sa.STL) +
  ggtitle("STL decomposition of billetes y monedas")


remes.sa.STL <- stl(remes,t.window=13, s.window="periodic", robust=TRUE)
remes.sa.STL.tremd <- trendcycle(remes.sa.STL)
remes.sa.STL.seasonal <- seasonal(remes.sa.STL)
remes.sa.STL.remainder <- remainder(remes.sa.STL)
remes.sa.STL.ajusted <- seasadj(remes.sa.STL)

remes.sa.STL %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") #Seasonal sub-series plot of the seasonal component from the SEATS decomposition 

autoplot(remes.sa.STL) +
  ggtitle("STL decomposition of billetes y monedas")


