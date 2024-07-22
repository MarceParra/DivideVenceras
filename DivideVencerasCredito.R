# Instalar las librerías
install.packages("C50")
install.packages("caret")
install.packages("tidyverse")

#Llamar a las librerias 
library(C50)
library(caret)
library(tidyverse)

#El conjunto de datos de crédito incluye 1000 ejemplos de préstamos,

archivo = "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/credit.csv"
file <- download.file(archivo, destfile = "credit.csv")
data <- read.csv("credit.csv", stringsAsFactors = T)

#Analisis exploratorio
data$default <- factor(data$default, levels = c(2,1), labels = c("yes", "no"))
table(data$default)

# par de características que podrían predecir el incumplimiento del pago
summary(data$months_loan_duration)
summary(data$amount)

#division de datos 
set.seed(123)
training_sample <- sample(1000, 900)
data_training <- data[training_sample,]
data_test <- data[-training_sample,]

prop.table(table(data_training$default))
prop.table(table(data_test$default))

#ajuste del modelo
modeloc50 <- C5.0(data_training[-17], data_training$default) 
summary(modeloc50)

#Evaluacion del modelo 
data_predicted <- predict(modeloc50, data_test)
confusionMatrix(data = data_predicted, reference = data_test$default, positive = "yes")

#Mejora del Modelo
modeloc50_1 <- C5.0(data_training[-17], data_training$default, trials = 10) 
data_predicted2 <- predict(modeloc50_1, data_test)
confusionMatrix(data = data_predicted2, reference = data_test$default, positive = "yes")

#matriz de costo
matrix_dimensions <- list(c("yes", "no"), c("yes", "no"))
names(matrix_dimensions) <- c("prediction", "reference")
error_cost <- matrix(c(0, 4, 1, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

#Matriz de confusion
modeloc50_costs <- C5.0(data_training[-17], data_training$default, costs = error_cost) 
data_predicted_costs <- predict(modeloc50_costs, data_test)
confusionMatrix(data = data_predicted_costs, reference = data_test$default, positive = "yes")

