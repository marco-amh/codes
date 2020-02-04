#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("H://Proyectos Especiales//Proyectos//COPOM DAM//2020//1. Febrero//GAM//_Consumo")
setwd("C://Users//D14371//Desktop")
#install.packages("mFilter")
library(dplyr)
library(lubridate)
library(readxl)
library(mFilter)
library(xlsx)
#-----------------------------------	
# (1) Prepara los datos	
#-----------------------------------	

#Importa los datos 
Base <- read_excel("Libro23.xlsx",sheet = "R_gap_pib")
#Base <- read_excel("Base M1 consumo.xlsx",sheet = "Base_R")
#Base <- read_excel("Base M1 consumo.xlsx",sheet = "Base", skip=2)

#Inversión fija bruta

ifb <- read_excel("Base M1 consumo.xlsx",sheet = "R")
ln.ifb <- log(ifb$IFB)
ifb.hp <- hpfilter(ln.ifb, freq = 129600 ,type = "lambda")
ifb.hp.cycle <- ifb.hp$cycle*100
write.csv(ifb.hp.cycle, "IFB_Brechas.csv")

#IGAE

igae <- read_excel("Base M1 consumo.xlsx",sheet = "R")
ln.igae <- log(igae$IGAE)
igae.hp <- hpfilter(ln.igae, freq = 129600 ,type = "lambda")
igae.hp.cycle <- igae.hp$cycle*100
write.csv(igae.hp.cycle, "igae_Brechas.csv")


#IGAE Desestacionalizado

igae.sa <- read_excel("Base M1 consumo.xlsx",sheet = "R")
ln.igae.sa <- log(igae.sa$IGAE_sa)
igae.sa.hp <- hpfilter(ln.igae.sa, freq = 129600 ,type = "lambda")
igae.sa.hp.cycle <- igae.sa.hp$cycle*100
write.csv(igae.sa.hp.cycle, "igae_sa_Brechas.csv")

#PIB desestacionalizado constantes

pib_sa <- read_excel("Base M1 consumo.xlsx",sheet = "R")
ln.pib <- log(pib_sa$PIB_sa)
pib.hp <- hpfilter(ln.pib, freq = 129600 ,type = "lambda")
pib.hp.cycle <- pib.hp$cycle*100
write.csv(pib.hp.cycle, "PIB_sa_Brechas.csv")

#Producción industrial sa

y_EU_sa <- read_excel("Base M1 consumo.xlsx",sheet = "R")
ln.y_EU_sa <- log(y_EU_sa$y_EU_sa)
y_EU_sa.hp <- hpfilter(ln.y_EU_sa, freq = 129600 ,type = "lambda")
y_EU_sa.hp.cycle <- y_EU_sa.hp$cycle*100
write.csv(y_EU_sa.hp.cycle, "y_EU_sa_Brechas.csv")


#Producción industrial sa

y_EU_sa <- read_excel("Libro23.xlsx",sheet = "R_gap_pib")
ln.y_EU_sa <- log(y_EU_sa$PIB_EU_nom_sa)
y_EU_sa.hp <- hpfilter(ln.y_EU_sa, freq = 129600 ,type = "lambda")
y_EU_sa.hp.cycle <- y_EU_sa.hp$cycle*100
write.csv(y_EU_sa.hp.cycle, "PIB_EU_nom_sa_Brechas.csv")



#-----------------------------------	
# (2) Loop para el cálculo de las brechas
#-----------------------------------	


#Se crea lista vacía para almacenar los resultados
lista <- list()

for (i in 1:ncol(Base)) {
  serie_i <- log(Base[,i])
  hp <- hpfilter(serie_i[[1]], freq = 129600 ,type = "lambda")
  serie_i = hp$cycle*100
  
  #Guardamos
  lista[[i]] <- serie_i
}




Base <- do.call(cbind.data.frame, lista)

write.csv(Base, "Brechas_2.csv")

