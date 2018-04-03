#Multiple Regression: Variable Preparation.

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

# Seleccion de subconjunto de variables para el modelo de regresión. 
auto.sel <- subset(auto, select = c(horsepower, city.mpg, peak.rpm, curb.weight, num.of.doors, price))

# Analisis de variables: Distribucion Normal, Multiple Colinearidad, Valores Extremos.
# Homoscedasticity (even distribution of residuals), p-value of coefficients and R2/F-statistic of the model

# Revisar por no linealidades y transformacion de variables. 
pairs.panels(auto.sel, col="red")
auto.sel$price <- log10(auto.sel$price)     #Cambio a escala logaritmica. 
auto.sel$horsepower <- log10(auto.sel$horsepower)
pairs.panels(auto.sel, col="red")

# Desarrollo de un modelo lineal.
# Construccion del modelo empleando el subconjunto de entrenamiento. 
# Validacion usando el subconjunto de validacion.

# Separar el conjunto de datos en entranamiento y validación.
# Training: (train.size)%.
# Validacion: (100-train.size)%.

set.seed(20)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

# Formula para describir la regresion multivariable: 
# Price = B0 + B1 x Horsepower + B2 x Curb.weight + B3 x City.mpg + ...

# Eleccion de las variables empleando una eliminación inversa. 
# Considerando todas las variables posibles y eliminando una por una. 
# 

# No consideraremos la interacción variable en esta etapa. 
# Modelo de: price ~ horsepower*curb.weight*city.mpg*Peak.rpm*num.of.doors

### Fit the model (1)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
# R-squared is a statistical measure of how close the data are to the fitted regression line. 
# It is also known as the coefficient of determination, or the coefficient of 
# multiple determination for multiple regression. # R^2=83.53

# The F-statistic is simply a ratio of two variances. Variances are a measure of dispersion, 
# or how far the data are scattered from the mean. # F=160.3
summary(fit) 
plot(fit)

# Analisis en busqueda de propiedades no lineales. Analisando el error del modelo lineal.
crPlots(fit)

# Elimina los valores extremos.
# Cook's D plot, cutoff as 4/(n-k-1)
# Useful for identifying outliers in the X values (observations for predictor variables).

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 

plot(fit, which=4, cook.levels=cutoff)   # Identificar D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("45", "127", "130")),]  #c("74", "91", "167")   

# Reajuste del modelo (2)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=86.95%, F=206.6

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("13", "17", "156")),] #c("22", "114", "197")

### Reajuste del modelo (3)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=87.99%, F=222

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("19", "75", "110")),] #c("35", "44", "125")  

### Reajuste del modelo (4)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=89%, F=236

# Revisar y eliminar los valores extremos.
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("15", "91", "111")),] 

### Reajuste del modelo (5)
fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=89.5%, F=248

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)

# Comprobacion para multicolinealidad con el Factor de Inflacion de Varianza
# Multicollinearity is a phenomenon in which one predictor variable in a multiple regression model 
# can be linearly predicted from the others with a substantial degree of accuracy.
# Variance inflation factor (VIF) quantifies how much the variance is inflated.
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)

# Reajuste del modelo, eliminar horsepower debido a la multicolinealidad. 
fit <- lm(price ~ curb.weight+peak.rpm+num.of.doors, data=train.sample)
summary(fit) # R2=89% but tough it was inflated, F=396
vif(fit)

# Reajuste del modelo - elimina num.of.doors due to p-value
fit <- lm(price ~ curb.weight+peak.rpm, data=train.sample)
summary(fit) # R2=88.79%, F=590
vif(fit)

### VIF, F-ratio and p-values say it is good, so no need to do anything else

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, 
                           newdata = subset(train.sample, select=c(price, peak.rpm, curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
                           newdata = subset(valid.sample, select=c(price, peak.rpm, curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)
# With all prep is: 0.8836 2670.0000 1759.0000 / As above
# Do nothing was:   0.7225 3997.0000 2676.0000 / See previous lesson

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$Price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# With all prep is: 0.7396 5723.0000 3334.0000 / As above
# Do nothing was:   0.6889 4927.0000 3208.0000 / See previous lesson

# Small data set - Cross-validation should be used, but vars selection needs to be auto!
# These results and the model should now be interpreted




