library(tidyverse)
library(readr)

library(dplyr)
library(plotly)
library(stringr)
library(tidyr)
library(highcharter)
library(writexl)



################################################################################3
df <- read.csv("tabla_completa.csv")
df <- df[,-1]


### CONOCIENDO EL DATA SET

str(df)
summary(df)

### LIMPIANDO


df$CLIENTE <- gsub("\\|", "&", df$CLIENTE )


df <- df %>%
  separate(CLIENTE, c("CLIENTE", "Status1"), "/")

df <- df %>%
  separate(CLIENTE, c("CLIENTE", "Status2"), "&&&")

df$Status1 <- gsub("\\&&&", "& ", df$Status1)

df$Status1 <- gsub(" ", "", df$Status1)


### COMBINANDO LAS 2 COLUMNAS DE STATUS
df <- df %>% 
  unite("Status", Status2:Status1, remove = F)

### QUEDANDOME SOLO CON LAS COLUMNAS QUE SI SON NA
df$Status <- gsub("\\_NA", "", df$Status )
df$Status <- gsub("\\NA_", "", df$Status )

### Confirmando que no hay mas missing values que los esperados
colSums(is.na(df))


#####################################################################################################################3
# AHORA EMPIEZA LO CHIDO

#Demanda parece estacional? En cuestion de cantidad de envios no en ventas
demanda_mensual <- df %>%
  select(MES) %>%
  group_by(MES) %>% 
  summarise(n = n())

demanda_mensual$Cambio <- ((demanda_mensual$n/lag(demanda_mensual$n) - 1) * 100)
demanda_mensual$Envios_promedio <- demanda_mensual$n / 9

mean(demanda_mensual$n)

df %>%
  select(MES) %>%
  group_by(MES) %>% 
  summarise(n = n()) %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Envios por mes </b>")

demanda_mensual %>%
  hchart("column", hcaes(x = MES, y = Cambio)) %>%
  hc_title(text = "<b>Envios por mes </b>")

demanda_mensual %>%
  hchart("column", hcaes(x = MES, y = Envios_promedio)) %>%
  hc_title(text = "<b>Envios que tuviera que hacer cada piloto en promedio </b>")


# Entregas en 2017 por piloto
entregas2017_ppiloto <- df %>%
  select(PILOTO) %>%
  group_by(PILOTO) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) 

entregas2017_ppiloto$Promedio <- 2180 / 9



df %>%
  select(PILOTO) %>%
  group_by(PILOTO) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  hchart("column", hcaes(x = PILOTO, y = n)) %>%
  hc_title(text = "<b>Entregas 2017 por Piloto </b>") %>%
  hc_subtitle(text = "<i>Fernando Berrio fue el que mas entrego y Juan Portillo el que menos </i>") 

# Entregas al mes
entregas_mes_ppiloto <- df %>%
  select(PILOTO, MES) %>%
  group_by(PILOTO,MES) %>% 
  summarise(n = n())

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Angel Valdez Alegria",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Angel Valdez Alegria </b>")
  

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Felipe Villatoro",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Felipe Villatoro </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Fernando Mariano Berrio",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Fernando Mariano Berrio </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO == "Hector Aragones Fruto",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Hector Aragones Fruto</b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Hector Giron",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Hector Giron </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Ismael Rodero Monteagudo",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Ismael Rodero Monteagudo </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="	Juan Francisco Portillo Gomez",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>	Juan Francisco Portillo Gomez </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Felipe Villatoro",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Felipe Villatoro </b>")

entregas_mes_ppiloto[entregas_mes_ppiloto$PILOTO =="Felipe Villatoro",] %>%
  hchart("column", hcaes(x = MES, y = n)) %>%
  hc_title(text = "<b>Felipe Villatoro </b>")


### NI MODO TODO A EXCEL

write_xlsx(df,"C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 4/df_listo.xlsx")
write_xlsx(demanda_mensual,"C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 4/demanda_mensual.xlsx")
write_xlsx(entregas_mes_ppiloto,"C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 4/entregas_mes_ppiloto.xlsx")
write_xlsx(entregas2017_ppiloto,"C:/Users/sergi/Desktop/Marro/Semestre 8/Wrangling/Git-Wrangling/Lab. 4/entregas2017.xlsx")










