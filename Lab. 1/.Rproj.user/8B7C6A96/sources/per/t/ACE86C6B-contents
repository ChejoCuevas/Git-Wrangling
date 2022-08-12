generate_df <- function(x, tamanio) (return(
  data.frame(
    a = sample(letters, size = tamanio, replace = T),
    b = sample(letters, size = tamanio, replace = T)
  )
  
))
generate_df(tamanio = 5)

lista <- lapply(X = 1:4, FUN = generate_df, tamanio=4)
lista

# Cargando Librerias
library(readxl)
library(readr)
library(tidyverse)
library(tidytext)

# Cargando los archivos

excel1 <- readxl::read_excel('01-2018.xlsx')
excel2 <- read_excel("02-2018.xlsx")
excel3 <- read_excel("03-2018.xlsx")
excel4 <- read_excel("04-2018.xlsx")
excel5 <- read_excel("05-2018.xlsx")
excel6 <- read_excel("06-2018.xlsx")
excel7 <- read_excel("07-2018.xlsx")
excel8 <- read_excel("08-2018.xlsx")
excel9 <- read_excel("09-2018.xlsx")
excel10 <- read_excel("10-2018.xlsx")
excel11 <- read_excel("11-2018.xlsx")

# Eliminando las columnas que no vamos a necesitar
excel1[, c('COD_VIAJE', 'CLIENTE', 'UBICACION', 'CANTIDAD', 'PILOTO', 'Q', 'CREDITO', 'UNIDAD')]
excel1 <- excel1[, c('COD_VIAJE', 'CLIENTE', 'UBICACION', 'CANTIDAD', 'PILOTO', 'Q', 'CREDITO', 'UNIDAD')]

generate_df <- function(x, tamanio) (return(
  data.frame(
    a = sample(letters, size = tamanio, replace = T),
    b = sample(letters, size = tamanio, replace = T)
  )
  
))
generate_df(tamanio = 5)

lista <- lapply(X = 1:4, FUN = generate_df, tamanio=4)
lista

limpiardf <- function(df) {
  
  df <- df[, c('COD_VIAJE', 'CLIENTE', 'UBICACION', 'CANTIDAD', 'PILOTO', 'Q', 'CREDITO', 'UNIDAD')]
  return(df)
}

agregar_fecha <- function(date, df ) {
  df$fecha <- substring(date, 1, 7)
  
  return(df)  
}

excel1 <- limpiardf(excel1)
ene18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/01-2018.xlsx")
excel1<-agregar_fecha(ene18,excel1)


excel2 <- limpiardf(excel2)
feb18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/02-2018.xlsx")
excel2<-agregar_fecha(feb18,excel2)


excel3 <- limpiardf(excel3)
mar18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/03-2018.xlsx")
excel3<-agregar_fecha(mar18,excel3)


excel4 <- limpiardf(excel4)
abr18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/04-2018.xlsx")
excel4<-agregar_fecha(abr18,excel4)


excel5 <- limpiardf(excel5)
may18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/05-2018.xlsx")
excel5<-agregar_fecha(may18,excel5)


excel6 <- limpiardf(excel6)
jun18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/06-2018.xlsx")
excel6<-agregar_fecha(jun18,excel6)


excel7 <- limpiardf(excel7)
jul18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/07-2018.xlsx")
excel7<-agregar_fecha(jul18,excel7)


excel8 <- limpiardf(excel8)
ago18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/08-2018.xlsx")
excel8<-agregar_fecha(ago18,excel8)


excel9 <- limpiardf(excel9)
sep18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/09-2018.xlsx")
excel9<-agregar_fecha(sep18,excel9)


excel10 <- limpiardf(excel10)
oct18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/10-2018.xlsx")
excel10<-agregar_fecha(oct18,excel10)


excel11 <- limpiardf(excel11)
nov18 <- basename(path = "C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 1/11-2018.xlsx")
excel11<-agregar_fecha(nov18,excel11)

# UNIENDO TODOS LOS DF EN UNO SOLO
excel_uni <- rbind(excel1, excel2, excel3, excel4, excel5, excel6, excel7, excel8, excel9, excel10, excel11)
excel_uni
  
# EXPORTANDO EL CSV
write_excel_csv2(excel_uni, 'excel_uni.xls', delim= ",")





