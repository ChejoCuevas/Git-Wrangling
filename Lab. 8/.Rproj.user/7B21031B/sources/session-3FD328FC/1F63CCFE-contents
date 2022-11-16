
# CARGANDO LAS LIBRERIAS Y LA DATA
library(dplyr)
library(gensvm)
titanic <- read.csv("titanic.csv")
titanic_MD <- read.csv("titanic_MD.csv")

######################################################################## Parte 1

# Reporte detallado de missing data para todas las columnas

summary(titanic_MD)


titanic_MD <- titanic_MD %>% 
                              mutate(Survived = factor(Survived),
                              Pclass = factor(Pclass),
                              Sex = factor(Sex),
                              SibSp = factor(SibSp),
                              Parch = factor(Parch),
                              Embarked = factor(Embarked)) %>% 
                              select(-PassengerId, -Name)
summary(titanic_MD)

# Para cada columna especificar qué tipo de modelo se utilizará (solo el
                                                      #nombre y el porqué) y qué valores se le darán a todos los missing values.

#Esta se encuentra en el reporte















