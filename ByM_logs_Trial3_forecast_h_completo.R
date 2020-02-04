# Este script estima los modelos de pronóstico con un componente estacional importante incluidos en:	
# Forecasting: Principles and Practice by Rob J Hyndman and George Athanasopouloswork	

# Está basado en: ByM_logs_Trial1_h1.R de Marco A. Martínez	

# Marco Martinez Huerta 3.10.19	


#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("H://Investigación Económica//JRH//daily_weekly_money_demand//")	
#install.packages("purrr") #sirve para juntar listas
library(purrr)
library(lubridate)	
library(seasonal)	
library(fpp2)	
library(xlsx) 
library(plyr) #Unir vectores de distintas matrices
#Data_frame <- reduce(Lista, bind_cols)  #une listas
#-----------------------------------	
# (1) Prepara los datos	
#-----------------------------------	

#Importa los datos con frecuencia semanal	
data <- read.csv("bym.csv", header=T);	

last_date <- as.numeric(last(data$Fecha)) #última semana de la muestra
class(last_date)

# Se crean las fechas 	
billetes.ts <- ts(data$ByM, start=decimal_date(ymd("2002-01-04")), frequency=365.25/7);	
date <- time(billetes.ts);	

# Transforma las variables en logaritmos.	
l.billetes.ts <- log(billetes.ts);	

#-----------------------------------	
# (2) Estimaciones e Inferencia	
#-----------------------------------	

# Define el punto inicial y final de la muestra para estimar	
start_date <- date[1]; # Usar si se comienza a estimar desde 2002

end_date <- last(date);	#última observación

# Define el tamaño de la muestra
n <- length(billetes.ts);

# Define el factor de aumento de semana por semana (convierte años en semanas)
factor <- (365.25/(365.25/7)/365.25);

# Define el número de semanas de acotamiento de la muestra
s <- 5  

# Define la fecha a partir de que se acota la muestra para hacer las estimaciones y realizar pronósticos
n0 <- start_date+(factor*(n-s))	

# Define el tamaño de la muestra truncada
n1 <- length(window(l.billetes.ts, start=start_date,end=start_date+(factor*(n-s))));

#Define el horizonte del pronóstico
h <- 52; 

# Define la dimensión de la matriz en la que se almacenan los resultados 
          
#nf <- n-n0-h+1;	
nf <- n-n1+1;

# Creación de matrices
fcast.fit4_fit     <- matrix(0,n,s)	#Matriz de NA's de 1*nf para almacenar los pronósticos de la metodología 4
fcast.fit4_res     <- matrix(0,n,s)	#Matriz de NA's de 1*nf para almacenar los pronósticos de la metodología 4
fcast.fit4_fcast     <- matrix(0,h,s)	#Matriz de NA's de 1*nf para almacenar los pronósticos de la metodología 4

fcast.fit7     <- matrix(NA,nf,1)	#Matriz de NA's de 1*nf para almacenar los pronósticos de la metodología 7
fcast.fit8     <- matrix(NA,nf,1)	#Matriz de NA's de 1*nf para almacenar los pronósticos de la metodología 8

list_4_fcast=list()
list_4_fit=list()
list_4_res=list()

list_7_fcast=list()
list_7_fit=list()
list_7_res=list()

list_8_fcast=list()
list_8_fit=list()
list_8_res=list()

j<-1 #factor de ayuda para hacer crecer el loop 	

# Loop para realizar las estimaciones y los pronósticos

for (i in seq(from=start_date+(factor*(n-s)), to=start_date+(factor*(n-1)), by=factor)){	
  
  print(paste("vamos en", i))	
  
  # Usa window() para obtener una muestra truncada	
  l.billetes.test <- window(l.billetes.ts, start=date[1],end=i);	
  
  
  
  # (B.3.4) Método TBATS	
  
  #  (B.3.4.1) Pronóstico TBATS	
  l.billetes.tbats.test <- tbats(l.billetes.test)	
  l.billetes.fit4 <- forecast(l.billetes.tbats.test, h=h)	
  
  #fcast.fit4_fit[,j]    <- l.billetes.fit4$fitted;	
  #fcast.fit4_res[,j]    <- l.billetes.fit4$residuals;
  #fcast.fit4_fcast[,j]    <- l.billetes.fit4$mean;
  
  list_4_fcast[[j]]   <- l.billetes.fit4$mean;
  list_4_fit[[j]]     <- l.billetes.fit4$fitted;
  list_4_res[[j]]     <- l.billetes.fit4$residuals;
  
  #(B.3.7) Método descomposición STFL	
  
  # (B.3.7.1) Estimación y pronóstico STFL	
  l.billetes.fit7 <- stlf(l.billetes.test, lambda="auto", h=h)	
  
  #fcast.fit7[j]    <- l.billetes.fit7$mean[h];	
  list_7_fcast[[j]]   <- l.billetes.fit7$mean;
  list_7_fit[[j]]     <- l.billetes.fit7$fitted;
  list_7_res[[j]]     <- l.billetes.fit7$residuals;
 
  #(B.3.8) Método Best ARIMA	
  
  # (B.3.8.1) Estimación y pronóstico Best ARIMA	
  bestfit <- list(aicc=Inf)	
  for(K in seq(25)) {	# K es cuántos pares de senos y cosenos se tiene. El máximo es k=52/2 y cuando se usa el máximo es idéntico como si se usaran dummies estacionales
    print(paste("vamos en", K))	
    fit <- auto.arima(l.billetes.test, xreg=fourier(l.billetes.test, K=K),	
                      seasonal=FALSE)	
    if(fit[["aicc"]] < bestfit[["aicc"]]) {	
      bestfit <- fit	
      bestK <- K	
    }	
  }	
  
  l.billetes.fit8 <- forecast(bestfit,	# Pronóstico Best.Auto ARIMA 
                              xreg=fourier(l.billetes.test, K=bestK, h=h))	
  
  #fcast.fit8[j]    <- l.billetes.fit8$mean[h];	
  
  list_8_fcast[[j]]   <- l.billetes.fit8$mean;
  list_8_fit[[j]]     <- l.billetes.fit8$fitted
  list_8_res[[j]]     <- l.billetes.fit8$residuals
  
  j <- j+1	
}	


#-----------------------------------	
# (3) Exportación de resultados
#-----------------------------------	

list_4_fit <- Reduce(cbind,list_4_fit)
list_4_res <- Reduce(cbind,list_4_res)
list_4_fcast <- Reduce(cbind,list_4_fcast)

list_7_fit <- Reduce(cbind,list_7_fit)
list_7_res <- Reduce(cbind,list_7_res)
list_7_fcast <- Reduce(cbind,list_7_fcast)

list_8_fit <- Reduce(cbind,list_8_fit)
list_8_res <- Reduce(cbind,list_8_res)
list_8_fcast <- Reduce(cbind,list_8_fcast)


lista_completa <- cbind(list_4_fit,list_7_fit,list_8_fit,list_4_res,list_7_res,list_8_res,list_4_fcast,list_7_fcast,list_8_fcast)

#names(lista_completa)[1] <- "TBATS_fit_291119"
lista_completa_exp<- exp(lista_completa)/1000

write.xlsx(lista_completa_exp, "forecasts_240120.xlsx")


#names(list_4_fit)=c("TBATS_fit_221119","TBATS_fit_291119","TBATS_fit_061219")
#names(list_4_res)=c("TBATS_res_221119","TBATS_res_291119","TBATS_res_061219")
#names(list_4_fcast)=c("TBATS_fcast_221119","TBATS_fcast_291119","TBATS_fcast_061219")

#names(list_7_fit)=c("STFL_fit_221119","STFL_fit_291119","SRFL_fit_061219")
#names(list_7_res)=c("STFL_res_221119","STFL_res_291119","SRFL_res_061219")
#names(list_7_fcast)=c("STFL_fcast_221119","STFL_fcast_291119","STFL_fcast_061219")

#names(list_8_fit)=c("ARIMA_fit_221119","ARIMA_fit_291119","ARIMA_fit_061219")
#names(list_8_res)=c("ARIMA_res_221119","STFL_res_291119","ARIMA_res_061219")
#names(list_8_fcast)=c("ARIMA_fcast_221119","STFL_fcast_291119","ARIMA_fcast_061219")


rbind.fill.matrix(list_7_fit)

list_7_fit <- Reduce(cbind,list_7_fit, list_8_fit)

write.xlsx(list_7_fit, "prueba_061219.xlsx")

Data_frame <- reduce(list_4_fit, bind_cols)  #une listas

Data_frame <- reduce(list_4_fit, bind_cols)  #une listas

list_4_fit_1 <- as.data.frame(list_4_fit[1])
names(list_4_fit)=c("TBATS_221119","TBATS_291119","TBATS_061219")
lista_completa <- do.call(c,list(list_4_fit,list_7_fit,list_8_fit, list_4_))

fcast.fit4_fit <- list_4_fit

class(fcast.fit4_fit)
Data_frame <- reduce(list_4_fit, bind_cols)  #une listas




do.call("merge", c(lapply(list(A, B), data.frame, row.names=NULL), by = 0, all = TRUE))[-1]

names(list_4_fit)=c("TBATS_221119","TBATS_291119","TBATS_061219")
names(list_4_fit)=c(paste0("TBATS",date[n-2]),"TBATS_291119","TBATS_061219")

lista_completa <- do.call(c,list(list_4,list_7,list_8))








write.xlsx(lista_completa, "fcast_291119.xlsx")

# Se crea un data frame de los vectores con los resultados almacenados
assign(paste0("fcast",sep="_","h",(h)),data.frame(f_m4 = fcast.fit4, f_m7 = fcast.fit7, f_m8 = fcast.fit8)) #Se utiliza assign para asignarle un valor a paste


# Almacena los resultados del ejericio en un archivo de .Rdata
save.image(paste0("ByM_logs_Trial2_h",(h),"_out_forecast_",Sys.Date(),".Rdata"))

# Exporta los resultados a un excel
write.xlsx(lista_completa, file=paste0("fcast_",(h),"_est_",Sys.Date(),".xlsx"))

