#Multiple Regression: Variable Selection.

#Data: https://archive.ics.uci.edu/ml/datasets/automobile
#This data set consists of three types of entities:
#(a) the specification of an auto in terms of various characteristics, 
#(b) its assigned insurance risk rating, 
#(c) its normalized losses in use as compared to other cars. 

#Regresion linear, regresion local, dependencia entre variables y diseño de modelos para predicciones. 

#Prediccion de los precios con base en las especificaciones del automovil. 
rm(list=ls()) 

library(Hmisc)
library(psych)
library(car)
library(ggplot2)
library(rgl)
library(gridExtra)
library(scales)

#Definir el directorio de trabajo
setwd("...")
auto<-read.csv(file="automobile_data.csv",header=TRUE,na.strings = "?")
str(auto)
summary(auto)
head(auto)
View(auto)

# Limpieza de las variables, NA. 
# Se intercambian los valores faltantes por el promedio del conjunto de datos.

auto$price<-as.numeric(impute(auto$price,mean))
auto$normalized.losses<-as.numeric(impute(auto$normalized.losses,mean))
auto$symboling<-as.numeric(impute(auto$symboling,median))

#Variables con valores faltantes
#Cuatro puertas ==> 1, Dos puertas ==>2
auto$num.of.doors<-as.numeric(impute(auto$num.of.doors,median))
auto$horsepower<-as.numeric(impute(auto$horsepower,mean))
auto$peak.rpm<-as.numeric(impute(auto$peak.rpm,mean))
auto$bore<-as.numeric(impute(auto$bore,mean))
auto$stroke<-as.numeric(impute(auto$stroke,mean))

summary(auto)
colnames(auto)

# Seleccion de un subconjunto de variables para el modelado de regresion
auto.sel <- subset(auto, select = c(horsepower, city.mpg, peak.rpm, curb.weight, num.of.doors, price))


# Analyse variables for: Distribucion Normal, Multiple Colinearidad, Valores Extremos.
# Homoscedasticity (even distribution of residuals)

# Inspeccion rapida de las variables involucradas.
pairs.panels(auto.sel, col="red")

##### Desarrollo del Modelo Lineal #####
# La construccion del modelo se hara con base en el subconjunto de entrenamiento.
# La validación se realizara usando el subconjunto de validación.

# Se divide la información en un subconjunto de entrenamiento y validación.
# Subconjunto de entrenamiento (train.size)
# Subconjunto de validación (100-train.size)%.
set.seed(5)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

### Multiple regression model utilises a simple formula:
# El modelo de regresion multivariable emplea una formula simple:
#    price = B0 + B1 x Horsepower + B2 x curb.weight + B3 x city.mpg

# Eleccion de las variables empleando una eliminación inversa. 
# Considerando todas las variables posibles y eliminando una por una.  

fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data=train.sample)
summary(fit) # Multiple R-squared:=77%

#Se elimina Numero de puertas
fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight, data=train.sample)
summary(fit) # Multiple R-squared:=77%

# Se elimina curb.weight
fit <- lm(price ~ horsepower+city.mpg+curb.weight, data=train.sample)
summary(fit) # Multiple R-squared:=77%

# Se elimina city.mpg
fit <- lm(price ~ horsepower+curb.weight, data=train.sample)
summary(fit) # Multiple R-squared:=76%
plot(fit)


# Los valores de R^2 practicamente no se modificaron

# Se encontraron valores extremos que pueden ser eliminados del modelo
train.sample[which(rownames(train.sample) %in% c("128", "17", "75")),] # c("167", "44", "91")

##### Evaluar el modelo final sin los valores extremos
# Encontrar todos los valores predecidos para el subconjunto de entrenamiento y validación.

train.sample$Pred.Price <- predict(fit, 
               newdata = subset(train.sample, select=c(price, horsepower, curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
               newdata = subset(valid.sample, select=c(price, horsepower, curb.weight)))

# Desempeño del modelo
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.Price - train.sample$price)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.Price - train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.7569 3853.0000 2767.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$price), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.Price - valid.sample$price)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.Price - valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.5929 5450.0000 3642.0000