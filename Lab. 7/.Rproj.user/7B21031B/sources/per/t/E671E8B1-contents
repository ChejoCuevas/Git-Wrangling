library(dplyr)
library(highcharter)
library(lubridate)
library(tidyverse)
library(readr)
library(plotly)
library(stringr)
library(openxlsx)
library(readxl)


# Cargando el csv y cambiandole nombre a las columnas
data <- read.csv2(file = "c1.csv",header = T,sep = ",")
data <- data[,1:22]
colnames(data)[22] <- 120
colnames(data)[18:21] <- c("5-30","30-45","45-75","75-120")


############################################################Conociendo el dataset

#Los montos estan en char y tienen un espacio
str(data)
head(data)
summary(data)

#Aplicados tipo de dato que en realidad es
data$origen<-as.factor(data$origen)
data$Cod<-as.factor(data$Cod)
data$Lat <- as.numeric(data$Lat)
data$Long <- as.numeric(data$Long)
data$Fecha <- dmy(data$Fecha)

#Pasando de Char a numeric los valores
data$factura <- gsub('\\ Q', "", data$factura)
data$factura <- as.numeric(data$factura)

data$Pickup <- gsub('\\ Q', "", data$Pickup)
data$Pickup <- as.numeric(data$Pickup)
data$Pickup <- replace(data$Pickup,is.na(data$Pickup), "0")
data$Pickup <- as.numeric(data$Pickup)
sum(is.na(data$Pickup))

data$Camion_5 <- gsub('\\ Q', "", data$Camion_5)
data$Camion_5 <- as.numeric(data$Camion_5)
data$Camion_5 <- replace(data$Camion_5,is.na(data$Camion_5), "0")
data$Camion_5 <- as.numeric(data$Camion_5)
sum(is.na(data$Camion_5))

data$Moto <- gsub('\\ Q', "", data$Moto)
data$Moto <- as.numeric(data$Moto)
data$Moto <- replace(data$Moto,is.na(data$Moto), "0")
data$Moto <- as.numeric(data$Moto)
sum(is.na(data$Moto))

data$directoCamion_5 <- gsub('\\ Q', "", data$directoCamion_5)
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
data$directoCamion_5 <- replace(data$directoCamion_5,is.na(data$directoCamion_5), "0")
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
sum(is.na(data$directoCamion_5))

data$directoPickup <- gsub('\\ Q', "", data$directoPickup)
data$directoPickup <- as.numeric(data$directoPickup)
data$directoPickup <- replace(data$directoPickup,is.na(data$directoPickup), "0")
data$directoPickup <- as.numeric(data$directoPickup)
sum(is.na(data$directoPickup))

data$directoMoto <- gsub('\\ Q', "", data$directoMoto)
data$directoMoto <- as.numeric(data$directoMoto)
data$directoMoto <- replace(data$directoMoto,is.na(data$directoMoto), "0")
data$directoMoto <- as.numeric(data$directoMoto)
sum(is.na(data$directoMoto))

data$fijoCamion_5 <- gsub('\\ Q', "", data$fijoCamion_5)
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
data$fijoCamion_5 <- replace(data$fijoCamion_5,is.na(data$fijoCamion_5), "0")
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
sum(is.na(data$fijoCamion_5))

data$fijoPickup <- gsub('\\ Q', "", data$fijoPickup)
data$fijoPickup <- as.numeric(data$fijoPickup)
data$fijoPickup <- replace(data$fijoPickup,is.na(data$fijoPickup), "0")
data$fijoPickup <- as.numeric(data$fijoPickup)
sum(is.na(data$fijoPickup))

data$fijoMoto <- gsub('\\ Q', "", data$fijoMoto)
data$fijoMoto <- as.numeric(data$fijoMoto)
data$fijoMoto <- replace(data$fijoMoto,is.na(data$fijoMoto), "0")
data$fijoMoto <- as.numeric(data$fijoMoto)
sum(is.na(data$fijoMoto))

# Creando sola columna de tiempo

data$'5-30' <- gsub('x', "1", data$'5-30')
data$'30-45' <- gsub('x', "1", data$'30-45')
data$'45-75' <- gsub('x', "1", data$'45-75')
data$'75-120' <- gsub('x', "1", data$'75-120')
data$'120' <- gsub('x', "1", data$'120')


data$'5-30' <- as.numeric(data$'5-30')
data$'30-45' <- as.numeric(data$'30-45')
data$'45-75' <- as.numeric(data$'45-75')
data$'75-120' <- as.numeric(data$'75-120')
data$'120' <- as.numeric(data$'120')

data$'5-30' <- gsub('1', "5-30", data$'5-30')
data$'30-45' <- gsub('1', "30-45", data$'30-45')
data$'45-75' <- gsub('1', "45-75", data$'45-75')
data$'75-120' <- gsub('1', "75-120", data$'75-120')
data$'120' <- gsub('1', "120", data$'120')

data$tiempo_recorrido <- coalesce(data$`5-30`,data$`30-45`,data$`45-75`, data$`75-120`,data$`120`)
max(data$directoMoto)


# Exportando dataset final para hacer las graficas en excel

#write.xlsx(x = data,'C:/Users/Chejo/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 7', sheetName = "Data_final")

#
data$costo <- rowSums(data[11:16])
data$marge <- data$factura - data$costo

factura_por_cod <- data %>%
  select(Cod, factura,marge) %>%
  group_by(Cod) %>% 
  summarise(Average = mean(factura), Sd = sd(factura), Min = min(factura), Max = max(factura), mediana = median(factura),
            Average_margen = mean(marge), Sd_margen = sd(marge), Min_margen = min(marge), Max_margen = max(marge), mediana_margen = median(marge))

# Cargando el dataset final, con arreglos

data_final <- read.xlsx(xlsxFile = "data_final.xlsx")
data_final <- data_final[,-c(12:17)]
data_final <- data_final[,-c(9:10)]
data_final <- as.numeric(data_final[,c(10:11)])
data_final$costo <- rowSums(data_final[10:11])
data_final <- data_final[,-c(3:5)]
data_final <- data_final[,-9]


#write.xlsx(x = data_final,'C:/Users/Chejo/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 7')
