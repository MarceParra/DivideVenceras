# Instalar las librerías
#install.packages("OneR")

#Llamar libreria
library(OneR)

mushrooms <- read.csv("~/Downloads/mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

#eliminar datos que no sirven
mushrooms$veil_type <- NULL
str(mushrooms)

#Tipos
table(mushrooms$type)

#datos aleatorios
set.seed(123)
train_sample <- sample(8124, 7000)
str(train_sample)

mushrooms_train <- mushrooms[train_sample, ]
mushrooms_test  <- mushrooms[-train_sample, ]

install.packages("RWeka")
library(RWeka) 

mushroom_1R <- OneR(type ~ ., data = mushrooms_train)
mushroom_1R

summary(mushroom_1R)

mushroom_pred <- predict(mushroom_1R, mushrooms_test)
library(gmodels)

CrossTable(mushrooms_test$type, mushroom_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

mushroom_JRip <- JRip(type ~ ., data = mushrooms_train)
mushroom_JRip

summary(mushroom_JRip)  

mushroom_pred <- predict(mushroom_JRip, mushrooms_test)

library(gmodels)
CrossTable(mushrooms_test$type, mushroom_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

library(C50)

mushroom_c5rules <- C5.0(type ~ odor + gill_size, 
                         data = mushrooms_train, rules = TRUE)
mushroom_c5rules

summary(mushroom_c5rules)

mushroom_pred <- predict(mushroom_c5rules, mushrooms_test)
library(gmodels)
CrossTable(mushrooms_test$type, mushroom_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

