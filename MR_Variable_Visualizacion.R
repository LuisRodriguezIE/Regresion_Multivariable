#Multiple Regression: Variable Visualización.

#Data: https://archive.ics.uci.edu/ml/datasets/automobile
#This data set consists of three types of entities:
#(a) the specification of an auto in terms of various characteristics, 
#(b) its assigned insurance risk rating, 
#(c) its normalized losses in use as compared to other cars. 

#Regresion linear, regresion local, dependencia entre variables,
# diseño de modelos para predicciones. 

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

### Select variables for regression modelling
auto.sel <- subset(auto, select = c(horsepower, city.mpg, peak.rpm, curb.weight, num.of.doors, price))

# Revisar por no linealidades y transformacion de variables. 
pairs.panels(auto.sel, col="red")
auto.sel$price <- log10(auto.sel$price)     #Cambio a escala logaritmica. 
auto.sel$horsepower <- log10(auto.sel$horsepower)
pairs.panels(auto.sel, col="blue")

# Separar el conjunto de datos en entranamiento y validación.
# Training: (train.size)%.
# Validacion: (100-train.size)%.

set.seed(20)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

# Elimina los valores extremos previamente conocidos por el analisis anterior. 
train.sample <- train.sample[-which(rownames(train.sample)    
                %in% c("12","13","15","17","19","45","75","91","110","111","127","130","156")),] 

# Crear el modelo que evita la multicolinealidad y revisa los valores p.
fit <- lm(price ~ curb.weight+peak.rpm, data=train.sample)
summary(fit) # R2=88.74%
vif(fit)

# Comprobar que tal se comporta el modelo empleando los valores correlation^2, RME and MAE
train.sample$Pred.Price <- predict(fit, 
                           newdata = subset(train.sample, select=c(price, peak.rpm, curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
                           newdata = subset(valid.sample, select=c(price, peak.rpm, curb.weight)))

# Subconjunto de entrenamiento
train.corr <- round(cor(train.sample$Pred.Price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)

# Subconjunto de validacion
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)

# Resultados:
# Conjunto de entrenamiento: 0.8836 2563.0000 1696.0000
# Conjunto de validacion: 0.6889 5366.0000 2978.0000

# Explorar los resultados empleando auto.sel
auto.explore <- subset(auto.sel, select=c(peak.rpm, curb.weight, price))
pairs.panels(auto.explore, col="red")

# Encontrar la relacion de peak y curb con el aumento de los precios.

# Definir el nivel de dependencia de precio en curb.weight
mstats <- summary(auto.explore$peak.rpm)
mstats
PrpmMin = as.numeric(mstats[1]) # Cambiar de dataframe a numeros
Prpm1Q = as.numeric(mstats[2])
PrpmMedian = as.numeric(mstats[3])
Prpm3Q = as.numeric(mstats[5])
PrpmMax = as.numeric(mstats[6])

# Generacion de puntos en el eje x en el rango de horsepower.
cwstats <- summary(auto.explore$curb.weight)
cwstats
CWRange = seq(from = 1400, to = 4100, by = 100)

# let us try two different linear models
fit <- lm(price ~ curb.weight+peak.rpm, data=train.sample)    # Modelo lineal
fit <- loess(price ~ curb.weight+peak.rpm, data=train.sample) # Modelo local

# Predict Prices at different Curb.weight levels
PredPriceMin = predict(fit, newdata=data.frame(peak.rpm=PrpmMin, curb.weight=CWRange))
PredPrice1Q = predict(fit, newdata=data.frame(peak.rpm=Prpm1Q, curb.weight=CWRange))
PredPriceMedian = predict(fit, newdata=data.frame(peak.rpm=PrpmMedian, curb.weight=CWRange))
PredPrice3Q = predict(fit, newdata=data.frame(peak.rpm=Prpm3Q, curb.weight=CWRange))
PredPriceMax = predict(fit, newdata=data.frame(peak.rpm=PrpmMax, curb.weight=CWRange))

# Grafica en el valor de precio transformado (recall log10 transform)
ggplot(auto.explore, aes(x=curb.weight, y=price)) + 
  ggtitle("Influencia de curb.weight y peak.rpm en el precio\n Regresion Multiple") +
  labs(x="Curb Weight + Nivel Peak RPM (lb)", y="Precio (log10 $)") +
  xlim(min(CWRange), max(CWRange)) +
  geom_point(color="gray", size=1, pch=19) +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredPriceMin), color="blue") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredPrice1Q), color="green") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredPriceMedian), color="yellow") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredPrice3Q), color="darkorange") +
  geom_line(data=data.frame(curb.weight=CWRange, price=PredPriceMax), color="red")

# Grafica en unidades de precio
ggplot(data.frame(curb.weight=auto.explore$curb.weight, price=10 ^ auto.explore$price), 
       aes(x=curb.weight, y=price)) + 
  ggtitle("Influencia de curb.weight y peak.rpm en el precio\n Regresion Multiple") +
  labs(x="Curb Weight + Nivel Peak RPM (lb)", y="Precio ($)") +
  xlim(min(CWRange), max(CWRange)) +
  geom_point(color="gray", size=1, pch=19) +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredPriceMin), color="blue") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredPrice1Q), color="green") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredPriceMedian), color="yellow") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredPrice3Q), color="darkorange") +
  geom_line(data=data.frame(curb.weight=CWRange, price=10 ^ PredPriceMax), color="red")

# Consider glm and gls, especially for logistic regression

##### Coplots
#     As we can see a single 2D plot is hampering our ability to see the details
#     Plot with Curb.weight variable as conditioning var
#     We plot with Curb.weight in different ranges and multiple plots

# Add regression lines to the plot
panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
  if(length(x)>4) {
    points(x, y, ...)
    abline(tmp, col="blue")
    panel.smooth(x, y, col.smooth="red", ...)
  } else {
    points(x, y, ...)
  }
}

# Plot with panels featuring regression and loess lines
coplot(price ~ curb.weight | peak.rpm, data=auto.explore, 
       col="orange", panel=panel.lm)


##### Graficas 3D 

### Impacto de la seleccion de las variables en el modelo.

scatter3d(x=auto$curb.weight, z=auto$peak.rpm, y=auto$price)
rgl.snapshot(filename = "Price_Peak_Curb.png")
scatter3d(x=auto$horsepower, z=auto$city.mpg, y=auto$price)
rgl.snapshot(filename = "Price_HP_City.png")
scatter3d(x=auto$horsepower, z=auto$curb.weight, y=auto$normalized.losses)
rgl.snapshot(filename = "Price_HP_City.png")

# Comparar la informacion limpia empleando solo el conjunto de entrenamiento. 

# Data sin ningun valor faltante. 
scatter3d(x=auto[train.index,]$curb.weight, 
          z=auto[train.index,]$peak.rpm, 
          y=auto[train.index,]$price)

# Data con las variables transformadas
scatter3d(x=auto.sel[train.index,]$curb.weight, 
          z=auto.sel[train.index,]$peak.rpm, 
          y=auto.sel[train.index,]$price)

# Data sin valores extremos
scatter3d(x=train.sample$curb.weight, 
          z=train.sample$peak.rpm, 
          y=train.sample$price)
rgl.snapshot(filename = "Regresion_SVE.png")


### Can we differentiate regression depending on the Price bracket, before data prep and after
#   Note that variables which were log transformed need to be delogged

# Define price breaks, row and cleaned up
Price.class <- ifelse(auto$price < 10000, "Low", ifelse(auto$price < 30000, "Medium", "High"))
Price.train.class <- ifelse(10^train.sample$price < 10000, "Low", ifelse(10^train.sample$price < 30000, "Medium", "High"))

# Multiple lineal regresion.
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(Price.class))
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(Price.train.class))
rgl.snapshot(filename = "Multiple_LR.png")

# LOESS regresion
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(Price.class), fit="smooth")
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(Price.train.class), fit="smooth")
rgl.snapshot(filename = "Multiple_LOESSR.png")

# Agrupacion por clases
scatter3d(price ~ peak.rpm * curb.weight, data=auto, groups=factor(Price.class), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)
scatter3d(10^price ~ peak.rpm * curb.weight, data=train.sample, groups=factor(Price.train.class), fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)

#FIN

