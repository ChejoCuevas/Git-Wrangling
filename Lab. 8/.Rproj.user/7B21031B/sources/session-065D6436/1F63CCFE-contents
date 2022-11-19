
# CARGANDO LAS LIBRERIAS Y LA DATA
library(dplyr)
library(gensvm)
library(Hmisc)
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

########################################### OBSERVANDO DISTRIBUCIONES INICIALES
summary(titanic_MD)

hist.data.frame(titanic_MD)


barplot(height = table(titanic_MD$Survived), col = "blue")
barplot(height = table(titanic_MD$Pclass), col = "blue")
barplot(height = table(titanic_MD$Sex), col = "blue")
barplot(height = table(titanic_MD$SibSp), col = "blue")
barplot(height = table(titanic_MD$Parch), col = "blue")
barplot(height = table(titanic_MD$Embarked), col = "blue")

hist(x = titanic_MD$Age)
hist(x = titanic_MD$Fare)



######################################### IMPUTANDO DATOS DE LA FORMA COMO SE ESPECIFIO EN EL REPORTE


# Sex


# Age


# SibSp


# Parch


# Fare


# EMbarked






