library(lubridate)
library(readxl)
library(nycflights13)
library(dplyr)
library(ggplot2)

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

# Arreglando la data
df <- df %>% rename(fecha_creacion = `Fecha Creación`,
                        hora_creacion = `Hora Creación`,
                        caller_id = `Caller ID`,
                        fecha_final = `Fecha Final`,
                        hora_final = `Hora Final`)
fecha_buena <- grepl(pattern = '-', x = df$fecha_creacion)
df1 <- df[fecha_buena,]
df2 <- df[!fecha_buena,]
df2 <- df2 %>% 
  mutate(fecha_creacion = as.Date(as.numeric(fecha_creacion), origin="1899-12-30"),
         fecha_final = as.Date(as.numeric(fecha_final), origin="1899-12-30")) %>% 
  mutate(fecha_creacion = ydm(fecha_creacion),
         fecha_final = ydm(fecha_final))
df1 <- df1 %>% 
  mutate(fecha_creacion = dmy(fecha_creacion),
         fecha_final = dmy(fecha_final))
df <- rbind(df1,df2)
df$duracion <- difftime(time1 = df$hora_final, 
                          time2 = df$hora_creacion, 
                          units="mins")

# a. ¿En qué meses existe una mayor cantidad de llamadas por código?

df$mes <- month(df$fecha_creacion)
por.mes <- df %>% group_by(mes, Cod) %>% 
  summarise(llamadas = n())

por.mes <- split(x = por.mes, f = por.mes$Cod)
por.mes <- lapply(por.mes, function(df) {df[order(df$llamadas, decreasing = T),]})
por.mes[[1]][1,]

por.mes[[2]][1,]

por.mes[[3]][1,]

por.mes[[4]][1,]

por.mes[[5]][1,]

por.mes[[6]][1,]

por.mes[[7]][1,]


# b. ¿Qué día de la semana es el más ocupado?

df$dia <- day(df$fecha_creacion)
por.dia <- df %>% group_by(dia) %>% summarise(llamadas=n())
por.dia[order(por.dia$llamadas, decreasing = T),][1,]


#c. ¿Qué mes es el más ocupado?

mensual <- df %>% group_by(mes) %>% summarise(llamadas=n())
mensual[order(mensual$llamadas, decreasing = T),][1,]

#d. ¿Existe una concentración o estacionalidad en la cantidad de llamadas?

g1 <- ggplot(data = mensual, mapping = aes(x=mes, y = llamadas))+
  geom_line()
g1

#e. ¿Cuántos minutos dura la llamada promedio?

mean(df$duracion)

#f. Realice una tabla de frecuencias con el tiempo de llamada

llamadas <- df %>%
  filter(Call == 1)%>%
  select((duracion))

Frecuancia_llamadas <- as.data.frame(table(llamadas))
names(Frecuancia_llamadas)[1] <- "Dur. llamada (min)"
names(Frecuancia_llamadas)[2] <- "Cant. de llamadas"
Frecuancia_llamadas


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






