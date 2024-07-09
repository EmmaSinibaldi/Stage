
rm(list =ls())



library(openxlsx)
library(dplyr)
library(readxl)
library(stringr)
library(rio)
library(ranger)
library(caret)
library(pdp)
library(randomForest)
library(FactoMineR)
library(glmnet)
library(corrplot)
library(ggcorrplot)
library(Factoshiny)
library(factoextra)
library(corrplot)
library(ggplot2)
library(Metrics)




#### RF pour spatialisation ####


data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
tab <- data[,c("Crop", "Asso_pure", "Yields", "N_fertilization_rate", "tot_rainfall", "veg_rainfall")] 

# Standardisation des predicteurs
X_quanti <- tab[, !names(tab) %in% c("Crop", "Asso_pure", "Yields")] 
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X_scale <- data.frame(apply(X_quanti,2,standard))
tab <- cbind(tab$Yields, tab$Crop, tab$Asso_pure, X_scale)
names(tab)[c(1,2,3)] <- c('Yields', 'Crop', 'Asso_pure')
str(tab)

# Random Forest avec l'ensemble des variables
set.seed(123)
inTrain <- createDataPartition(tab$Yields, p=0.8, list=FALSE)
train <- tab[inTrain,]
test <- tab[-inTrain,]




# grille pour hyperparametres

hyperparam <- expand.grid(
  mtry = 1:6,
  min.node.size = c(1, 3, 5, 10),
  splitrule = c("extratrees", "variance")
)

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

grid_search <- train(
  Yields ~ .,
  data = train,
  method = "ranger",
  trControl = control,
  tuneGrid = hyperparam,
  importance = "impurity",
  num.trees= 300
)

grid_search$bestTune

final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
                      num.trees = 300, mtry = grid_search$bestTune$mtry,
                      splitrule = grid_search$splitrule,
                      min.node.size = grid_search$bestTune$min.node.size, num.threads = 4)

final_model

# R2 : 0.978
# MSE : 0.04



# R2 sur ensemble de test
test$prediction <- predict(final_model, test)$predictions
(cor(test$Yields, test$prediction))^2
# 0.98


# sauver le modèle 
save(final_model, file = "C:/Users/sinibaldi/Desktop/GitHub/Stage/RF_model.RData")

