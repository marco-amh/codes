# Este script realiza la conversión de series de tiempo de baja frecencia a alta frecuencia

#Se utiliza el package tempdisagg

# Marco Martinez Huerta 24.10.19	


#-----------------------------------	
# (0) Limpia memoria y llama paquetes	
#-----------------------------------	

rm(list=ls())  	#Limpia las variables
cat("\014")	    #Limpia la consola

setwd("C://Users//D14371//Desktop//Carpeta de utilidades//Pronóstico mensual bym//Octubre//R mensualización//")	

#install.packages("tempdisagg")
library(tempdisagg)	

#-----------------------------------	
# (1) Prepara los datos	
#-----------------------------------	

#Importa los datos con frecuencia trimestral	
data <- read.csv("Datos_2019_10.csv", header=T);

#-----------------------------------	
# (2) Convertir a alta frecuencia	
#-----------------------------------

high_freq_tc <- ts(data$tc, start=2007, frequency=4);	

model_tc <- td(high_freq_tc ~ 1, conversion = "last", to = "monthly", method = "chow-lin-maxlog")

high_freq_ts_tc <- predict(model_tc)

?td

#-----------------------------------	
# (3) Exportación de resultados
#-----------------------------------

write.xlsx(high_freq_ts_tc, file="monthly_201910.xlsx")

