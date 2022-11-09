# LABORATORIO 6

library(stringr)


# Ejercicio 1
# Genere una expresión regular que sea capaz de detectar 
# las placas de un vehículo particular guatemalteco


placa <- c('P741JKC')
estructura <- str_detect(placa, '^[P]{1}[0-9]{3}(?![AEIOUÑ])[A-Z]{3}$')
placa[estructura==1]

placas <- c('P741JKC', 'O789ABC', 'P7H84HT', 'P123QWE','753CTB')
estructura2 <- str_detect(placas, '^[P]{1}[0-9]{3}(?![AEIOUÑ])[A-Z]{3}$')
placas[estructura2]



#Ejercicio 2
# 2.	Genere una expresión regular que valide si un 
# archivo es de tipo .pdf o jpg.

ejemplos <- c('Ejemplo1.pdf','Ejemp.csv', 'prueba2.PDF', 'plo.ppt', 'respuestas_del_examen.jpg', 'amor.JPG')
estruc <- grepl(pattern = "[.](jpg|JPG|pdf|PDF)$", x = ejemplos)
ejemplos[estruc]

#Ejercicio 3
# 3.	Genere una expresión regular para validar contraseñas
# de correo. Una contraseña de correo debe contener por lo
# menos 8 caracteres, una letra mayúscula y un carácter 
# especial

contra <- c('Contrasena', 'Contrasena1', 'Ccontrasena&1', 'Chejo&','Chejo&%753')
valid <- str_detect(pattern = '^(?=.*[a-z])(?=.*[A-Z])(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$', 
                     string = contra)
contra[valid==1]


#Ejercicio 5
# 5.	Cree una expresión regular que encuentre todas las 
# palabras de la primera línea, pero ninguna de la segunda.

#Probando otra forma!
palabras <- c('pit', 'spot', 'spate', 'slap two', 'respite', 'pt', 'Pot', 'peat', 'part')

y <- str_extract(palabras,'.*p.t.*')
y <- y[!is.na(y)]
y


# Ejercicio 7
# 7.	Genere una expresión regular que sea capaz de 
# obtener correos de la UFM.

correo <- c('sergiocuevas@ufm.edu', 'chejox@gmail.com', 'prueba@ufm.ed', 'holamundo@ufm.edu.gt')
validacion <- !is.na(str_extract(correo, '[A-Za-z]+@[ufm]+.(edu)$'))
correo[validacion==1]


# Ejercicio 8
# 8.	En el mundo distópico de Eurasia, Big Brother le 
# asigna un identificador único a cada ciudadano. Genere 
# una expresión regular que valide las identificaciones. 

# CONDICIONES>>>>
# a.	El id inicia con 0 a 3 letras minúsculas (puede tener 0 letras minúsculas hasta tres letras minúsculas)
# b.	Luego es seguido por una cadena de dígitos que puede ser de 2 a 9 dígitos respectivamente.
# c.	Inmediatamente después de la cadena de dígitos, se encuentra por lo menos tres letras mayúsculas.
# d.	Ej: abc012333ABCDEEEE

id <- c('ac733ABEEE','we852123478ERT75', '7584632ASUC', 'p8ERDFRG', 're67jhGTDU0H')
v <- str_detect(id,'^[a-z]{0,3}[0-9]{2,9}[A-Z]{2,}$')

id[v==1]







