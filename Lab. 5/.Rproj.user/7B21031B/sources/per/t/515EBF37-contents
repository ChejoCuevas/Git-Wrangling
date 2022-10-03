library(lubridate)
library(readxl)
library(nycflights13)
library(dplyr)

df <- read_excel('data.xlsx')
# PARTE 1 - ECLIPSE

eclipse_1 <- mdy_hms("August 21, 2017, 18:26:40", tz = "UTC")
eclipse_1

synodic_month <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)
synodic_month

saros <- 223 * synodic_month
saros

#EL Proximo eclipse sera el:
eclipse_2 <- eclipse_1 + saros
eclipse_2


# Parte 2 - Agrupaciones y operaciones con fechas
?strftime()


# a. ¿En qué meses existe una mayor cantidad de llamadas por código?
df$mes <- month(df$fecha_creacion)
por.mes <- data %>% group_by(mes, Cod) %>% 
  summarise(llamadas = n())




# Parte 3 - Signo Zodiacal
signo_zodiaco <- function(nacimiento) {
  fecha <- dmy(nacimiento)
  mes <- month(fecha)
  dia <- day(fecha)
  
  if(mes == 1) {
    signo <- ifelse(test = dia < 20,
                    yes = 'Capricornio',
                    no = 'Acuario')
  }
  else if(mes == 2) {
    signo <- ifelse(test = dia < 19,
                    yes = 'Acuario',
                    no = 'Piscis')
  }
  else if(mes == 3) {
    signo <- ifelse(test = dia < 21,
                    yes = 'Piscis',
                    no = 'Aries')
  }
  else if(mes == 4) {
    signo <- ifelse(test = dia < 20,
                    yes = 'Aries',
                    no = 'Tauro')
  }
  else if(mes == 5) {
    signo <- ifelse(test = dia < 21,
                    yes = 'Tauro',
                    no = 'Geminis')
  }
  else if(mes == 6) {
    signo <- ifelse(test = dia < 21,
                    yes = 'Geminis',
                    no = 'Cancer')
  }
  else if(mes == 7) {
    signo <- ifelse(test = dia < 23,
                    yes = 'Cancer',
                    no = 'Leo')
  }
  else if(mes == 8) {
    signo <- ifelse(test = dia < 23,
                    yes = 'Leo',
                    no = 'Virgo')
  }
  else if(mes == 9) {
    signo <- ifelse(test = dia < 23,
                    yes = 'Virgo',
                    no = 'Libra')
  }
  else if(mes == 10) {
    signo <- ifelse(test = dia < 23,
                    yes = 'Libra',
                    no = 'Escorpio')
  }
  else if(mes == 11) {
    signo <- ifelse(test = dia < 22,
                    yes = 'Escorpio',
                    no = 'Sagitario')
  }
  else if(mes == 12) {
    signo <- ifelse(test = dia < 22,
                    yes = 'Sagitario',
                    no = 'Capricornio')
  }
  
  return(signo)
}
#Escribe la fecha en la que naciste para saber tu signo zodiacal (dia-mes-anio)
signo_zodiaco(03052000)


# Parte 4 - Flights
flights %>% 
  select(dep_time, arr_time, sched_dep_time, sched_arr_time) %>%
  View()

flights$time_dep <- format(strptime(sprintf('%04d',flights$dep_time),
                                    format = '%H%M'),
                                    '%H:%M')
flights$time_arr <- format(strptime(sprintf('%04d',flights$arr_time),
                                    format = '%H%M'),
                           '%H:%M')
flights$time_sched_dep <- format(strptime(sprintf('%04d',flights$sched_dep_time),
                                    format = '%H%M'),
                           '%H:%M')
flights$time_sched_arr <- format(strptime(sprintf('%04d',flights$sched_arr_time),
                                    format = '%H%M'),
                           '%H:%M')
head(flights)

#B.	Encuentre el delay total que existe en cada vuelo.
#El delay total se puede encontrar sumando el delay de la salida y el delay de la entrada.

flights$delay_total <- flights$dep_delay + flights$arr_delay

flights %>% 
  select(time_dep, time_arr, time_sched_dep, time_sched_arr, delay_total) %>%
  View()






