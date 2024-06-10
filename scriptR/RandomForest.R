
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






#### Random Forest tableau entier ####

data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
tab <- data[,-c(1,4,5)] # Cropping_situation/Site/Year

# Standardisation des predicteurs
X_quanti <- tab[,-(1:3)] # Crop/Asso_pure/Yields
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

ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 300,
                 mtry = 5, splitrule ='variance', min.node.size = 1)
ranger

# R2 : 0.98
# MSE : 0.03


# # grille pour hyperparametres
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 300
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 300, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# 
# # R2 : 0.98
# # MSE : 0.03

ggcorrplot(cor(train[,-c(2,3)]))

# graphique convergence erreur
# erreur <- numeric(500)
# for ( i in 1:500){
#   ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = i,
#                    mtry = 5, splitrule ='variance', min.node.size = 1)
#   erreur[i] <- ranger$prediction.error
# }
# 
# erreur_data <- data.frame(num_trees = 1:500, erreur = erreur)
# ggplot(erreur_data, aes(x = num_trees, y=erreur)) + geom_line() + labs(x = 'Nombre arbres', y ='Erreur') + 
#   geom_vline(xintercept =300, col= "red")



# graphique des valeurs predites vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="black", linewidth = 1)+
  geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()

# graphique importance des variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + 
  labs(y = 'Variables') 
#Crop/N_fertilization_rate/veg_rainfall/Sowing_density/Sowing-date/tot_rainfall


# graphique de dépendance partielle des variables les plus importantes
pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()

pdp_veg_rainfall <- partial(ranger, pred.var = "veg_rainfall")
mean_veg_rainfall <- mean(data$veg_rainfall)
sd_veg_rainfall <- sd(data$veg_rainfall)
pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()

# boxplot
plot <- test
plot$fitted <- predict(ranger, data = test)$predictions
ggplot(plot, aes(x=Crop, y=fitted, color=Crop)) + geom_boxplot()+ facet_wrap(~Asso_pure)


# autres graphiques de dependance partielle
pdp_PAWC <- partial(ranger, pred.var = "PAWC")
mean_PAWC <- mean(data$PAWC)
sd_PAWC <- sd(data$PAWC)
pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()


pdp_Soil_N_organic <- partial(ranger, pred.var = "Soil_N_organic")
mean_Soil_N_organic <- mean(data$Soil_N_organic)
sd_Soil_N_organic <- sd(data$Soil_N_organic)
pdp_Soil_N_organic$Soil_N_organic <- pdp_Soil_N_organic$Soil_N_organic * sd_Soil_N_organic + mean_Soil_N_organic
ggplot(pdp_Soil_N_organic, aes(x = Soil_N_organic, y= yhat)) + geom_line()

pdp_nb_days_0 <- partial(ranger, pred.var = "nb_days_0")
mean_nb_days_0 <- mean(data$nb_days_0)
sd_nb_days_0 <- sd(data$nb_days_0)
pdp_nb_days_0$nb_days_0 <- pdp_nb_days_0$nb_days_0 * sd_nb_days_0 + mean_nb_days_0
ggplot(pdp_nb_days_0, aes(x = nb_days_0, y= yhat)) + geom_line()


pdp_Interrang <- partial(ranger, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()

pdp_critical_rainfall <- partial(ranger, pred.var = "critical_rainfall")
mean_critical_rainfall <- mean(data$critical_rainfall)
sd_critical_rainfall <- sd(data$critical_rainfall)
pdp_critical_rainfall$critical_rainfall <- pdp_critical_rainfall$critical_rainfall * sd_critical_rainfall + mean_critical_rainfall
ggplot(pdp_critical_rainfall, aes(x = critical_rainfall, y= yhat)) + geom_line()


pdp_nb_days_60 <- partial(ranger, pred.var = "nb_days_60")
mean_nb_days_60 <- mean(data$nb_days_60)
sd_nb_days_60 <- sd(data$nb_days_60)
pdp_nb_days_60$nb_days_60 <- pdp_nb_days_60$nb_days_60 * sd_nb_days_60 + mean_nb_days_60
ggplot(pdp_nb_days_60, aes(x = nb_days_60, y= yhat)) + geom_line()



# ACP sur l'ensemble des variables explicatives








rm(list =ls())


#### Random Forest cereales (pures et associees) ####


data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
cereales <- subset(data, (Crop == 'Sorghum' | Crop == 'Millet'))
cereales <- cereales[,-c(1,4,5)] #Cropping_situation/Site/Year

X_quanti <- cereales[,-c(1,2,3)] #Crop/Asso_pure/Yields

# Standardisation des predicteurs
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X_quanti,2,standard))
Xacp <- cbind(cereales$Crop, cereales$Asso_pure, X)
names(Xacp)[c(1,2)] <- c('Crop', 'Asso_pure')

cereales <- cbind(cereales$Yields, cereales$Crop, cereales$Asso_pure, X)
names(cereales)[c(1,2,3)] <- c('Yields', 'Crop', 'Asso_pure')

str(cereales)


# rf 
set.seed(123)
inTrain <- createDataPartition(cereales$Yields, p=0.8, list=FALSE)
train <- cereales[inTrain,]
test <- cereales[-inTrain,]

ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500,
                 mtry= 5, splitrule ='variance', min.node.size=1)
ranger
# R2 : 0.97
# MSE : 0.04


# avec grille pour hyperparametres
# 
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.969
# MSE : 0.04




# graphique valeurs predites vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="black", linewidth=1)+
  geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()

# predictions <- predict(ranger, data = test)$predictions
# plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Asso = test$Asso_pure)
# ggplot(plot_data, aes(x= Yields, y = Prediction, color = Asso)) +
#   geom_point() +
#   geom_abline(intercept =0, slope = 1, color="black", linewidth=1)+
#   geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
#   labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()


# importance des variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables') 
#N_fertilization_rate/veg_rainfall/tot_rainfall


# graphiques de dépendance partielle des variables les plus importantes
pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()

pdp_veg_rainfall <- partial(ranger, pred.var = "veg_rainfall")
mean_veg_rainfall <- mean(data$veg_rainfall)
sd_veg_rainfall <- sd(data$veg_rainfall)
pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()

pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()

# boxplot
plot <- test
plot$fitted <- predict(ranger, data = test)$predictions
ggplot(plot, aes(x=Crop, y=fitted, color=Crop)) + geom_boxplot()+ facet_wrap(~Asso_pure)



# graphiques de dependance partielle autres variables
pdp_Soil_N_organic <- partial(ranger, pred.var = "Soil_N_organic")
mean_Soil_N_organic <- mean(data$Soil_N_organic)
sd_Soil_N_organic <- sd(data$Soil_N_organic)
pdp_Soil_N_organic$Soil_N_organic <- pdp_Soil_N_organic$Soil_N_organic * sd_Soil_N_organic + mean_Soil_N_organic
ggplot(pdp_Soil_N_organic, aes(x = Soil_N_organic, y= yhat)) + geom_line()

pdp_nb_days_0 <- partial(ranger, pred.var = "nb_days_0")
mean_nb_days_0 <- mean(data$nb_days_0)
sd_nb_days_0 <- sd(data$nb_days_0)
pdp_nb_days_0$nb_days_0 <- pdp_nb_days_0$nb_days_0 * sd_nb_days_0 + mean_nb_days_0
ggplot(pdp_nb_days_0, aes(x = nb_days_0, y= yhat)) + geom_line()

pdp_Interrang <- partial(ranger, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()

pdp_PAWC <- partial(ranger, pred.var = "PAWC")
mean_PAWC <- mean(data$PAWC)
sd_PAWC <- sd(data$PAWC)
pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_Sowing_density <- partial(ranger, pred.var = "Sowing_density")
mean_Sowing_density <- mean(data$Sowing_density)
sd_Sowing_density <- sd(data$Sowing_density)
pdp_Sowing_density$Sowing_density <- pdp_Sowing_density$Sowing_density * sd_Sowing_density + mean_Sowing_density
ggplot(pdp_Sowing_density, aes(x = Sowing_density, y= yhat)) + geom_line()

pdp_rep_rainfall <- partial(ranger, pred.var = "rep_rainfall")
mean_rep_rainfall <- mean(data$rep_rainfall)
sd_rep_rainfall <- sd(data$rep_rainfall)
pdp_rep_rainfall$rep_rainfall <- pdp_rep_rainfall$rep_rainfall * sd_rep_rainfall + mean_rep_rainfall
ggplot(pdp_rep_rainfall, aes(x = rep_rainfall, y= yhat)) + geom_line()



# acp variables explicatives
acp <- PCA(Xacp, scale.unit = F, quali.sup = c(1,2))
fviz_eig(acp, addlabels = TRUE)
acp$eig[1:10,]

# cumulative_variance <- cumsum(acp$eig[,2])
# plot(1:length(cumulative_variance),cumulative_variance, type="b", xlab = "nb de composantes",
#      ylab = "Pourcentage de variance expliquée", main = "Pourcentage de variance expliquée par nombre de composantes")
# point <- which(diff(cumulative_variance) < 1)[1]
# abline(v = point, col = "red", lty = 2)

Comp1 <- acp$ind$coord[,1]
Comp2 <- acp$ind$coord[,2]
Comp3 <- acp$ind$coord[,3]
Comp4 <- acp$ind$coord[,4]

comp_cer <- data.frame(C1 = Comp1, C2=Comp2, C3 = Comp3, C4 =Comp4)
comp_cer <- cbind(cereales$Yields, comp_cer, cereales$Crop, cereales$Asso_pure)
names(comp_cer)[1] <- 'Yields'
names(comp_cer)[6] <- 'Crop'
names(comp_cer)[7] <- 'Asso_pure'

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)

set.seed(123)
inTrain <- createDataPartition(comp_cer$Yields, p=0.8, list=FALSE)
train <- comp_cer[inTrain,]
test <- comp_cer[-inTrain,]
ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500,
                  mtry = 3, splitrule = 'variance', min.node.size = 1)
ranger
# 0.95



# avec grille pour hyperparametres

# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)

# final_model


# graphique preditions vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="black", linewidth=1)+
  geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()


# graphique importance des variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables') 


# graphiques de dépendance partielle 
pdp_C1 <- partial(ranger, pred.var = "C1")
plot(pdp_C1, type = 'l')
ggplot(pdp_C1, aes(x = C1, y= yhat)) + geom_line()

pdp_C2 <- partial(ranger, pred.var = "C2")
plot(pdp_C2, type = 'l')
ggplot(pdp_C2, aes(x = C2, y= yhat)) + geom_line()

pdp_C3 <- partial(ranger, pred.var = "C3")
plot(pdp_C3, type = 'l')
ggplot(pdp_C3, aes(x = C3, y= yhat)) + geom_line()

pdp_C4 <- partial(ranger, pred.var = "C4")
plot(pdp_C4, type = 'l')
ggplot(pdp_C4, aes(x = C4, y= yhat)) + geom_line()




#### Random Forest cereales associees avec variable rendements pures ####
# 
# data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
# 
# rendements_cer <- read.csv("C:/Users/sinibaldi/Desktop/stage/dataframes/rendements_cer.csv", sep = ";", dec = ".")
# 
# cereales2 <- subset(data, (Crop == 'Sorghum' | Crop == 'Millet') & Asso_pure == 'associee')
# 
# df <- merge(cereales2, rendements_cer, by  ='Cropping_situation')
# df <- df[, -c(1,3,4,5,21)]
# df_init <- df
# 
# X_quanti <- df[, -c(1,2)]
# 
# 
# # Standardisation des predicteurs
# variance <- function(x){sum((x-mean(x))^2)/length(x)}
# standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
# X <- data.frame(apply(X_quanti,2,standard))
# 
# Xacp <- cbind(cereales2$Crop, X)
# names(Xacp)[1] <- 'Crop'
# 
# df <- cbind(cereales2$Yields, cereales2$Crop, X)
# names(df)[c(1,2)] <- c('Yields','Crop')
# 
# 
# # random forest avec l'ensemble des variables
# 
# set.seed(123)
# inTrain <- createDataPartition(df$Yields, p=0.8, list=FALSE)
# train <- df[inTrain,]
# test <- df[-inTrain,]
# 
# 
# ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 400,
#                  splitrule ='variance', mtry= 5, min.node.size=1)
# ranger 
# 
# # R2 : 0.95
# # MSE : 0.07
# 
# 
# 
# 
# # avec grille pour hyperparametres
# # 
# # 
# # hyperparam <- expand.grid(
# #   mtry = 1:5,
# #   min.node.size = c(1, 3, 5, 10),
# #   splitrule = c("extratrees", "variance")
# # )
# # 
# # control <- trainControl(method ="cv", number = 10)
# # 
# # grid_search <- train(
# #   Yields ~ .,
# #   data = train,
# #   method = "ranger",
# #   trControl = control,
# #   tuneGrid = hyperparam,
# #   importance = "impurity",
# #   num.trees= 400
# # )
# # 
# # grid_search$bestTune
# # 
# # final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
# #                       num.trees = 400, grid_search$bestTune$mtry,
# #                       min.node.size = grid_search$bestTune$min.node.size)
# # 
# # final_model
# # R2 : 0.948
# # MSE : 0.07
# 
# 
# 
# 
# # graphique valeures prédites vs vraies valeures 
# predictions <- predict(ranger, data = test)$predictions
# plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
# ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
#   geom_point() +
#   geom_abline(intercept =0, slope = 1, color="red", linewidth =1 )+
#   geom_smooth(method = "lm", se = FALSE, col = "blue", linewidth =1)+
#   labs(x = "Vraies valeurs de Yields", y = "Predictions") +
#   ggtitle("Predictions vs vraies valeurs ") + coord_equal()
# 
# 
# 
# 
# 
# 
# # variable importantes
# variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
# data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
# ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
#   geom_bar(stat = "identity") + labs(y = 'Variables') 
# # Nfertilization rate / Yields pure / Crop
# 
# 
# 
# # graphique de dependance partielle des variables les plus  simportantes
# 
# pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
# plot(pdp_N_fertilization_rate, type = 'l')
# mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
# sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
# pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
# plot(pdp_N_fertilization_rate, type = 'l')
# ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()
# 
# 
# 
# pdp_Yields_pure <- partial(ranger, pred.var = "Yields_pure")
# mean_Yields_pure <- mean(df_init$Yields_pure)
# sd_Yields_pure <- sd(df_init$Yields_pure)
# pdp_Yields_pure$Yields_pure <- pdp_Yields_pure$Yields_pure * sd_Yields_pure + mean_Yields_pure
# ggplot(pdp_Yields_pure, aes(x = Yields_pure, y= yhat)) + geom_line()
# 
# 
# 
# 
# # autres graphiques de dépendance partielle
# 
# pdp_Soil_N_organic <- partial(ranger, pred.var = "Soil_N_organic")
# mean_Soil_N_organic <- mean(df_init$Soil_N_organic)
# sd_Soil_N_organic <- sd(df_init$Soil_N_organic)
# pdp_Soil_N_organic$Soil_N_organic <- pdp_Soil_N_organic$Soil_N_organic * sd_Soil_N_organic + mean_Soil_N_organic
# ggplot(pdp_Soil_N_organic, aes(x = Soil_N_organic, y= yhat)) + geom_line()
# 
# pdp_Sowing_density <- partial(ranger, pred.var = "Sowing_density")
# mean_Sowing_density <- mean(df_init$Sowing_density)
# sd_Sowing_density <- sd(df_init$Sowing_density)
# pdp_Sowing_density$Sowing_density <- pdp_Sowing_density$Sowing_density * sd_Sowing_density + mean_Sowing_density
# ggplot(pdp_Sowing_density, aes(x = Sowing_density, y= yhat)) + geom_line()
# 
# 
# pdp_PAWC <- partial(ranger, pred.var = "PAWC")
# mean_PAWC <- mean(df_init$PAWC)
# sd_PAWC <- sd(df_init$PAWC)
# pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
# ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()
# 
# 
# pdp_nb_days_0 <- partial(ranger, pred.var = "nb_days_0")
# mean_nb_days_0 <- mean(df_init$nb_days_0)
# sd_nb_days_0 <- sd(df_init$nb_days_0)
# pdp_nb_days_0$nb_days_0 <- pdp_nb_days_0$nb_days_0 * sd_nb_days_0 + mean_nb_days_0
# ggplot(pdp_nb_days_0, aes(x = nb_days_0, y= yhat)) + geom_line()
# 
# 
# pdp_Interrang <- partial(ranger, pred.var = "Interrang")
# mean_Interrang <- mean(df_init$Interrang)
# sd_Interrang <- sd(df_init$Interrang)
# pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
# ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()
# 
# 
# pdp_nb_days_60 <- partial(ranger, pred.var = "nb_days_60")
# mean_nb_days_60 <- mean(df_init$nb_days_60)
# sd_nb_days_60 <- sd(df_init$nb_days_60)
# pdp_nb_days_60$nb_days_60 <- pdp_nb_days_60$nb_days_60 * sd_nb_days_60 + mean_nb_days_60
# ggplot(pdp_nb_days_60, aes(x = nb_days_60, y= yhat)) + geom_line()
# 
# 
# 
# 
# # acp 
# 
# acp <- PCA(Xacp, scale.unit = F, quali.sup = c(1))
# fviz_eig(acp, addlabels = TRUE)
# acp$eig[1:10,]
# 
# 
# Comp1 <- acp$ind$coord[,1]
# Comp2 <- acp$ind$coord[,2]
# Comp3 <- acp$ind$coord[,3]
# Comp4 <- acp$ind$coord[,4]
# 
# comp <- data.frame(C1 = Comp1, C2=Comp2, C3 = Comp3, C4 =Comp4)
# comp <- cbind(cereales2$Yields, comp, cereales2$Crop)
# names(comp)[1] <- 'Yields'
# names(comp)[6] <- 'Crop'
# 
# corrplot(acp$var$cos2, is.corr=F)
# corrplot(acp$var$contrib, is.corr=F)
# 
# set.seed(123)
# inTrain <- createDataPartition(comp$Yields, p=0.8, list=FALSE)
# train <- comp[inTrain,]
# test <- comp[-inTrain,]
# ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500,
#                  mtry = 3, splitrule = 'variance', min.node.size = 1)
# ranger
# 
# 
# 
# 
# 
# # avec grille pour hyperparametres
# 
# #
# # hyperparam <- expand.grid(
# #   mtry = 1:5,
# #   min.node.size = c(1, 3, 5, 10),
# #   splitrule = c("extratrees", "variance")
# # )
# #
# # control <- trainControl(method ="cv", number = 10)
# #
# # grid_search <- train(
# #   Yields ~ .,
# #   data = train,
# #   method = "ranger",
# #   trControl = control,
# #   tuneGrid = hyperparam,
# #   importance = "impurity",
# #   num.trees= 500
# # )
# #
# # grid_search$bestTune
# #
# # final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
# #                       num.trees = 500, grid_search$bestTune$mtry,
# #                       min.node.size = grid_search$bestTune$min.node.size)
# 
# # final_model
# 
# 
# 
# predictions <- predict(ranger, data = test)$predictions
# plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
# ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
#   geom_point() +
#   geom_abline(intercept =0, slope = 1, color="black", linewidth=1)+
#   geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
#   labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()
# 
# 
# 
# 
# #vip
# variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
# data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
# ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
#   geom_bar(stat = "identity") + labs(y = 'Variables') 
# 
# 
# #graphiques de dépendance partielle des variables les plus importantes
# 
# pdp_C1 <- partial(ranger, pred.var = "C1")
# plot(pdp_C1, type = 'l')
# ggplot(pdp_C1, aes(x = C1, y= yhat)) + geom_line()
# 
# 
# pdp_C2 <- partial(ranger, pred.var = "C2")
# plot(pdp_C2, type = 'l')
# ggplot(pdp_C2, aes(x = C2, y= yhat)) + geom_line()
# 
# 
# 
# pdp_C3 <- partial(ranger, pred.var = "C3")
# plot(pdp_C3, type = 'l')
# ggplot(pdp_C3, aes(x = C3, y= yhat)) + geom_line()
# 
# pdp_C4 <- partial(ranger, pred.var = "C4")
# plot(pdp_C4, type = 'l')
# ggplot(pdp_C4, aes(x = C4, y= yhat)) + geom_line()










rm(list = ls())

#### Random Forest legumineuses pures ####

data <- read.csv("data/donnees.csv", sep = ";", dec = ".")

leg_pure <- subset(data, Crop == 'Cowpea' & Asso_pure == 'pure')
leg_pure <- leg_pure[,-c(1,2,3,4,5,7)] #Cropping_situaton/Crop/Asso_pure/Site/Year/Ferti
leg_pure2 <- leg_pure
X <- leg_pure[,-1]

# Standardisation des predicteurs
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X,2,standard))
leg_pure <- cbind(leg_pure$Yields, X)
names(leg_pure)[1] <- 'Yields'

str(leg_pure)


# random forest avec l'ensemble des variables
set.seed(123)
inTrain <- createDataPartition(leg_pure$Yields, p=0.8, list=FALSE)
train <- leg_pure[inTrain,]
test <- leg_pure[-inTrain,]


ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 100, mtry = 2,
                 min.node.size = 1, splitrule = 'variance')
ranger
# R2 : 0.9602
# MSE : 0.00885


#valeurs prédites vs vrais valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions)
ggplot(plot_data, aes(x= Yields, y = Prediction)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "blue")+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") +
  ggtitle("Predictions vs vraies valeurs ") + coord_equal()



# avec grille pour hyperparametres
# 
# set.seed(123)
# inTrain <- createDataPartition(leg_pure$Yields, p=0.8, list=FALSE)
# train <- leg_pure[inTrain,]
# test <- leg_pure[-inTrain,]
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 100
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 100, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.9588
# MSE : 0.00885


# correlation variables
ggcorrplot(cor(train))
cor(leg_pure$tot_rainfall, leg_pure$veg_rainfall)

# graphique importance variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables') 
# rep_rainfall/tot_rainfall/interrang/sowing_date/critical_rainfall


# pdp des variables importantes
pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()

pdp_rep_rainfall <- partial(ranger, pred.var = "rep_rainfall")
mean_rep_rainfall <- mean(data$rep_rainfall)
sd_rep_rainfall <- sd(data$rep_rainfall)
pdp_rep_rainfall$rep_rainfall <- pdp_rep_rainfall$rep_rainfall * sd_rep_rainfall + mean_rep_rainfall
ggplot(pdp_rep_rainfall, aes(x = rep_rainfall, y= yhat)) + geom_line()

pdp_critical_rainfall <- partial(ranger, pred.var = "critical_rainfall")
mean_critical_rainfall <- mean(data$critical_rainfall)
sd_critical_rainfall <- sd(data$critical_rainfall)
pdp_critical_rainfall$critical_rainfall <- pdp_critical_rainfall$critical_rainfall * sd_critical_rainfall + mean_critical_rainfall
ggplot(pdp_critical_rainfall, aes(x = critical_rainfall, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_Interrang <- partial(ranger, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()


# ranger <- ranger(Yields~.-nb_days_60, data = train, importance = "impurity", num.trees = 100, mtry = 2,
#                  min.node.size = 1, splitrule = 'variance')
# ranger



# acp de l'ensemble des variables

acp <- PCA(X, scale.unit = FALSE)
fviz_eig(acp, addlabels = TRUE)
acp$eig[1:8,]

# acp <- PCA(X, scale.unit = F, ncp=2)
acp <- PCA(X, scale.unit = F, ncp=4)
comp_leg_pure <- data.frame(as.matrix(X) %*% acp$svd$V)

# leg_pure$CC1 <- acp$ind$coord[,1]
# leg_pure$CC2 <- acp$ind$coord[,2]
# leg_pure$CC3 <- acp$ind$coord[,3]
# leg_pure$CC4 <- acp$ind$coord[,4]


# cumulative_variance <- cumsum(acp$eig[,2])
# plot(1:length(cumulative_variance),cumulative_variance, type = "b", xlab = "nombre de composantes",
#      ylab = "Pourcentage de variance expliquée", main = "Pourcentage de variance expliquée par nombre de composantes")
# point <- which(diff(cumulative_variance) < 1)[1]
# abline(v = point, col = "red", lty = 2)


cor.test(comp_leg_pure$X1,comp_leg_pure$X2)
cor.test(comp_leg_pure$X1,comp_leg_pure$X3)
cor.test(comp_leg_pure$X2,comp_leg_pure$X3)
cor.test(comp_leg_pure$X3,comp_leg_pure$X4)

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)

comp_leg_pure <- cbind(leg_pure$Yields, comp_leg_pure)
names(comp_leg_pure)[1] <- 'Yields'

ggcorrplot(cor(comp_leg_pure))


# rf sur composantes principales
set.seed(123)
inTrain <- createDataPartition(comp_leg_pure$Yields, p=0.8, list=FALSE)
train <- comp_leg_pure[inTrain,]
test <- comp_leg_pure[-inTrain,]

ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 100, mtry = 4,
                 min.node.size = 3, splitrule = 'extratrees')
ranger
# R2 : 0.965
# MSE : 0.007


#valeurs prédites vs vrais valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions)
ggplot(plot_data, aes(x= Yields, y = Prediction)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "blue")+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") +
  ggtitle("Predictions vs vraies valeurs ") + coord_equal()


# importance variables
graphranger <- sort(ranger$variable.importance, decreasing=TRUE)
barplot(graphranger, horiz=T,las=2,cex.names=0.6, main='importance des variables')
acp$var$coord

pdp_X1 <- partial(ranger, pred.var = "X1")
plot(pdp_X1, type='l')
pdp_X2 <- partial(ranger, pred.var = "X2")
plot(pdp_X2, type='l')
pdp_X3 <- partial(ranger, pred.var = "X3")
plot(pdp_X3, type='l')
pdp_X4 <- partial(ranger, pred.var = "X4")
plot(pdp_X4, type='l')



# avec grille

# set.seed(123)
# inTrain <- createDataPartition(comp_leg_pure$Yields, p=0.8, list=FALSE)
# train <- comp_leg_pure[inTrain,]
# test <- comp_leg_pure[-inTrain,]
# 
# hyperparam <- expand.grid(
#   mtry = 1:4,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 100
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 100, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# final_model

# R2 : 0.9416
# MSE : 0.01







rm(list = ls())


#### Random Forest legumineuses associees ####

data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
leg_asso <- subset(data, Crop == 'Cowpea' & Asso_pure == 'associee')
leg_asso <- leg_asso[,-(1:5)] 
X <- leg_asso[,-1]

# Standardisation
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X,2,standard))

# data legumineuse associées standardisé
leg_asso <- cbind(leg_asso$Yields, X)
names(leg_asso)[1] <- 'Yields'

str(leg_asso)

# random forest avec lensemble des variables
set.seed(123)
inTrain1 <- createDataPartition(leg_asso$Yields, p=0.8, list=FALSE)
train1 <- leg_asso[inTrain1,]
test1 <- leg_asso[-inTrain1,]
ranger1 <- ranger(Yields~., data = train1, importance = "impurity", num.trees = 300,
                  mtry = 5, min.node.size = 1, splitrule = 'variance')
ranger1
# R2 : 0.981
# MSE : 0.0003


# avec grille pour hyperparametres
# 
# set.seed(123)
# inTrain1 <- createDataPartition(leg_asso$Yields, p=0.8, list=FALSE)
# train1 <- leg_asso[inTrain1,]
# test1 <- leg_asso[-inTrain1,]
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train1,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train1, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.98
# MSE : 0.0032





# scatter plot predictions/vraies valeurs
predictions <- predict(ranger1, data = test1)$predictions
plot_data <- data.frame(Yields = test1$Yields, Prediction = predictions)
ggplot(plot_data, aes(x= Yields, y = Prediction)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "blue")+
  labs(x = "Vraies valeurs de Yields", y = "Prédictions") +
  ggtitle("predictions vs vraies valeurs") + coord_equal()


#correlation entre les variables
ggcorrplot(cor(train1))

# importance des variables
graphranger1 <- sort(ranger1$variable.importance, decreasing=TRUE)
barplot(graphranger1, horiz=T,las=2,cex.names=0.6)


#pdp variables les plus importantes
pdp_tot_rainfall <- partial(ranger1, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()

pdp_critical_rainfall <- partial(ranger1, pred.var = "critical_rainfall")
mean_critical_rainfall <- mean(data$critical_rainfall)
sd_critical_rainfall <- sd(data$critical_rainfall)
pdp_critical_rainfall$critical_rainfall <- pdp_critical_rainfall$critical_rainfall * sd_critical_rainfall + mean_critical_rainfall
ggplot(pdp_critical_rainfall, aes(x = critical_rainfall, y= yhat)) + geom_line()

pdp_veg_rainfall <- partial(ranger1, pred.var = "veg_rainfall")
plot(pdp_veg_rainfall, type = 'l')
mean_veg_rainfall <- mean(data$veg_rainfall)
sd_veg_rainfall <- sd(data$veg_rainfall)
pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
plot(pdp_veg_rainfall, type = 'l')
ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()

pdp_N_fertilization_rate <- partial(ranger1, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()


# autres graphiques de dependance partielle

pdp_Interrang <- partial(ranger1, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()

pdp_Sowing_density <- partial(ranger1, pred.var = "Sowing_density")
mean_Sowing_density <- mean(data$Sowing_density)
sd_Sowing_density <- sd(data$Sowing_density)
pdp_Sowing_density$Sowing_density <- pdp_Sowing_density$Sowing_density * sd_Sowing_density + mean_Sowing_density
ggplot(pdp_Sowing_density, aes(x = Sowing_density, y= yhat)) + geom_line()


pdp_Sowing_date <- partial(ranger1, pred.var = "Sowing_date")
plot(pdp_Sowing_date, type = 'l')
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
plot(pdp_Sowing_date, type = 'l')
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_PAWC <- partial(ranger1, pred.var = "PAWC")
mean_PAWC <- mean(data$PAWC)
sd_PAWC <- sd(data$PAWC)
pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()


# acp de l'ensemble des variables 
acp <- PCA(X, scale.unit = F)

fviz_eig(acp, addlabels = TRUE)
acp$eig[1:10,]

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)

# acp <- PCA(X, scale.unit = F, ncp = 3)
acp <- PCA(X, scale.unit = F, ncp = 4)
comp_leg_asso <- data.frame(as.matrix(X) %*% acp$svd$V)
comp_leg_asso <- cbind(leg_asso$Yields, comp_leg_asso)
names(comp_leg_asso)[1] <- 'Yields'

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)

set.seed(123)
inTrain2 <- createDataPartition(comp_leg_asso$Yields, p=0.8, list=FALSE)
train2 <- comp_leg_asso[inTrain2,]
test2 <- comp_leg_asso[-inTrain2,]
ranger2 <- ranger(Yields~., data = train2, importance = "impurity", num.trees = 500,
                  mtry = 2, splitrule = 'variance', min.node.size = 1)
ranger2
# R2 : 0.976
# MSE : 0.0038


# avec grille pour hyperparametres
# 
# set.seed(123)
# inTrain2 <- createDataPartition(comp_leg_asso$Yields, p=0.8, list=FALSE)
# train2 <- comp_leg_asso[inTrain2,]
# test2 <- comp_leg_asso[-inTrain2,]
# 
# hyperparam <- expand.grid(
#   mtry = 1:4,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train2,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train2, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.976
# MSE : 0.0004



#correlation entre les variables
ggcorrplot(cor(comp_leg_asso))


#vip
graphranger2 <- sort(ranger2$variable.importance, decreasing=TRUE)
barplot(graphranger2, horiz=T,las=2,cex.names=0.6)


# graphique valeurs predites vs vraies valeurs
predictions <- predict(ranger2, data = test2)$predictions
plot_data <- data.frame(Yields = test2$Yields, Prediction = predictions)
ggplot(plot_data, aes(x= Yields, y = Prediction)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "blue")+
  labs(x = "Vraies valeurs de Yields", y = "Prédictions") +
  ggtitle("predictions vs vraies valeurs") + coord_equal()


# vip
graphranger2 <- sort(ranger2$variable.importance, decreasing=TRUE)
barplot(graphranger2, horiz=T,las=2,cex.names=0.6)


#pdp
pdp_X1 <- partial(ranger2, pred.var = "X1")
plot(pdp_X1, type='l')

pdp_X2 <- partial(ranger2, pred.var = "X2")
plot(pdp_X2, type='l')

pdp_X3 <- partial(ranger2, pred.var = "X3")
plot(pdp_X3, type='l')

pdp_X4 <- partial(ranger2, pred.var = "X4")
plot(pdp_X4, type='l')


  



rm(list = ls())


#### Random Forest cereales associees ####

# Chargement des donnees
data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
cer_asso <- subset(data, (Crop == 'Sorghum' | Crop == 'Millet') & Asso_pure == 'associee')
cer_asso <- cer_asso[,-c(1,3,4,5)]


# Standardisation
X_quanti <- cer_asso[,-c(1,2)]
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X_quanti,2,standard))
X <- cbind(cer_asso$Crop, X)
names(X)[1] <- 'Crop'

# data legumineuse associées standardisé
cer_asso <- cbind(cer_asso$Yields, X)
names(cer_asso)[1] <- 'Yields'

str(cer_asso)

# random forest avec l'ensemble des variables
set.seed(123)
inTrain1 <- createDataPartition(cer_asso$Yields, p=0.8, list=FALSE)
train1 <- cer_asso[inTrain1,]
test1 <- cer_asso[-inTrain1,]
ranger1 <- ranger(Yields~., data = train1, importance = "impurity", num.trees = 300,
                  mtry = 5, splitrule = 'variance', min.node.size = 1)
ranger1
# R2 : 0.9665
# MSE : 0.04





# avec grille pour hyperparametres

# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train1,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train1, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model

# R2 : 0.9669
# MSE : 0.04




# valeurs prédites vs vraies valeurs
predictions <- predict(ranger1, test1)$predictions
plot_data <- data.frame(Yields = test1$Yields, Prediction = predictions, Crop = test1$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color= Crop) ) +
  geom_point() +
  scale_color_manual(values = c("blue", "purple")) +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "black")+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") +
  ggtitle("Predictions vs vraies valeurs") + scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(0, 5)) + coord_equal()


# vip
variable_importance <- sort(ranger1$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables', title='Importance des variables') 
#fertilization_rate/veg_rainfall/tot_rainfall

# correlation variables
cor <- data.frame(cor(train1[,-2]))
ggcorrplot(cor(train1[,-2])) + ggtitle('Corrélations entre les variables')

# pdp variables importantes
pdp_N_fertilization_rate <- partial(ranger1, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()

pdp_veg_rainfall <- partial(ranger1, pred.var = "veg_rainfall")
mean_veg_rainfall <- mean(data$veg_rainfall)
sd_veg_rainfall <- sd(data$veg_rainfall)
pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()

pdp_tot_rainfall <- partial(ranger1, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()



# autre graphique
pdp_nb_days_60 <- partial(ranger1, pred.var = "nb_days_60")
mean_nb_days_60 <- mean(data$nb_days_60)
sd_nb_days_60 <- sd(data$nb_days_60)
pdp_nb_days_60$nb_days_60 <- pdp_nb_days_60$nb_days_60 * sd_nb_days_60 + mean_nb_days_60
ggplot(pdp_nb_days_60, aes(x = nb_days_60, y= yhat)) + geom_line()
 
pdp_Interrang <- partial(ranger1, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger1, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_nb_days_60 <- partial(ranger1, pred.var = "nb_days_60")
mean_nb_days_60 <- mean(data$nb_days_60)
sd_nb_days_60 <- sd(data$nb_days_60)
pdp_nb_days_60$nb_days_60 <- pdp_nb_days_60$nb_days_60 * sd_nb_days_60 + mean_nb_days_60
ggplot(pdp_nb_days_60, aes(x = nb_days_60, y= yhat)) + geom_line()

pdp_PAWC <- partial(ranger1, pred.var = "PAWC")
mean_PAWC <- mean(data$PAWC)
sd_PAWC <- sd(data$PAWC)
pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()


plot <- test1
plot$fitted <- predict(ranger1, data = test1)$predictions
ggplot(plot, aes(x=Crop, y=fitted, color=Crop)) + geom_boxplot()



## sans interrang
# 
# 
# set.seed(123)
# inTrain1 <- createDataPartition(cer_asso$Yields, p=0.8, list=FALSE)
# train1 <- cer_asso[inTrain1,]
# test1 <- cer_asso[-inTrain1,]
# ranger1 <- ranger(Yields~.-Interrang, data = train1, importance = "impurity", num.trees = 300,
#                   mtry = 5, splitrule = 'variance', min.node.size = 1)
# ranger1
# R2 : 0.968
# MSE : 0.04



# boxplot
# 
# ggplot(cer_asso, aes(x=Crop, y=Yields)) + geom_boxplot() + ggtitle('Rendement par type de culture')
# ggplot(plot, aes(x=Crop, y=fitted)) + geom_boxplot() + ggtitle('Rendement estimé par type de culture')




# acp des variables 

# Factoshiny(X)
# res.PCA<-PCA(X,quali.sup=c(1),graph=FALSE)
# plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
# plot.PCA(res.PCA,title="Graphe des individus de l'ACP")

acp <- PCA(X, scale.unit = F, quali.sup = c(1))
fviz_eig(acp, addlabels = TRUE)
acp$eig[1:10,]


Comp1 <- acp$ind$coord[,1]
Comp2 <- acp$ind$coord[,2]
Comp3 <- acp$ind$coord[,3]
Comp4 <- acp$ind$coord[,4]

comp_cer_asso <- data.frame(C1 = Comp1, C2=Comp2, C3 = Comp3, C4 =Comp4)
comp_cer_asso <- cbind(cer_asso$Yields, comp_cer_asso, cer_asso$Crop)
names(comp_cer_asso)[1] <- 'Yields'
names(comp_cer_asso)[6] <- 'Crop'

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)

set.seed(123)
inTrain2 <- createDataPartition(comp_cer_asso$Yields, p=0.8, list=FALSE)
train2 <- comp_cer_asso[inTrain2,]
test2 <- comp_cer_asso[-inTrain2,]
ranger2 <- ranger(Yields~., data = train2, importance = "impurity", num.trees = 500,
                  mtry = 2, splitrule = 'variance', min.node.size = 1)
ranger2
# R2 : 0.955
# MSE : 0.0667


# avec grille pour hyperparametres
# 
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train2,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train2, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.955
# MSE : 0.06




#corrélation entre les variables


# graphique importance des variables
variable_importance <- sort(ranger2$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables', title='Importance des variables') 



# valeurs predite vs vraies valeurs
predictions <- predict(ranger2, data = test2)$predictions
plot_data <- data.frame(Yields = test2$Yields, Prediction = predictions, Crop = test2$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color= Crop)) +
  scale_color_manual(values = c("blue", "purple")) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "black")+
  labs(x = "Vraies valeurs de Yields", y = "Prédictions") +
  ggtitle("predictions vs vraies valeurs") +  coord_equal()

acp$var$coord

# pdp
pdp_C1 <- partial(ranger2, pred.var = "C1")
ggplot(pdp_C1, aes(x = C1, y= yhat)) + geom_line()

pdp_C2 <- partial(ranger2, pred.var = "C2")
ggplot(pdp_C2, aes(x = C2, y= yhat)) + geom_line()

pdp_C3 <- partial(ranger2, pred.var = "C3")
ggplot(pdp_C3, aes(x = C3, y= yhat)) + geom_line()

pdp_C4 <- partial(ranger2, pred.var = "C4")
ggplot(pdp_C4, aes(x = C4, y= yhat)) + geom_line()






rm(list = ls())

#### Random Forest cereales pures ####


# Chargement des donnees
data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
cer_pure <- subset(data, (Crop == 'Sorghum' | Crop == 'Millet') & Asso_pure == 'pure')
cer_pure <- cer_pure[,-c(1,3,4,5)]

# Standardisation
X_quanti <- cer_pure[,-c(1,2)]
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X_quanti,2,standard))
X <- cbind(cer_pure$Crop, X)
names(X)[1] <- 'Crop'

# data legumineuse associées standardisé
cer_pure <- cbind(cer_pure$Yields, X)
names(cer_pure)[1] <- 'Yields'



# random forest avec l'ensemble des variables
set.seed(123)
inTrain <- createDataPartition(cer_pure$Yields, p=0.8, list=FALSE)
train <- cer_pure[inTrain,]
test <- cer_pure[-inTrain,]
ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 300,
                 mtry = 5, splitrule = 'variance', min.node.size = 1)
ranger
# R2 : 0.9636
# MSE : 0.05


# avec grille pour hyperparametres
# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.9669
# MSE : 0.04




# valeurs predite vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
  geom_point() +
  scale_color_manual(values = c("blue", "purple")) +
  geom_abline(intercept =0, slope = 1, color="red")+
  geom_smooth(method = "lm", se = FALSE, col = "black")+
  labs(x = "Vraies valeurs de Yields", y = "Prédictions") +
  ggtitle("Predictions vs vraies valeurs") + coord_equal()  


# corrélation entre les variables
ggcorrplot(cor(train[,-2]))

# importance des variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables', title='Importance des variables') 

# graphiques de dépendances partielles des variables les plus importantes
pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()

pdp_veg_rainfall <- partial(ranger, pred.var = "veg_rainfall")
mean_veg_rainfall <- mean(data$veg_rainfall)
sd_veg_rainfall <- sd(data$veg_rainfall)
pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()

pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()


# autres graphiques de dependance partielle
pdp_Soil_N_organic <- partial(ranger, pred.var = "Soil_N_organic")
mean_Soil_N_organic <- mean(data$Soil_N_organic)
sd_Soil_N_organic <- sd(data$Soil_N_organic)
pdp_Soil_N_organic$Soil_N_organic <- pdp_Soil_N_organic$Soil_N_organic * sd_Soil_N_organic + mean_Soil_N_organic
ggplot(pdp_Soil_N_organic, aes(x = Soil_N_organic, y= yhat)) + geom_line()

pdp_Sowing_density <- partial(ranger, pred.var = "Sowing_density")
mean_Sowing_density <- mean(data$Sowing_density)
sd_Sowing_density <- sd(data$Sowing_density)
pdp_Sowing_density$Sowing_density <- pdp_Sowing_density$Sowing_density * sd_Sowing_density + mean_Sowing_density
ggplot(pdp_Sowing_density, aes(x = Sowing_density, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()


pdp_Interrang <- partial(ranger, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()


pdp_nb_days_0 <- partial(ranger, pred.var = "nb_days_0")
mean_nb_days_0 <- mean(data$nb_days_0)
sd_nb_days_0 <- sd(data$nb_days_0)
pdp_nb_days_0$nb_days_0 <- pdp_nb_days_0$nb_days_0 * sd_nb_days_0 + mean_nb_days_0
ggplot(pdp_nb_days_0, aes(x = nb_days_0, y= yhat)) + geom_line()


pdp_critical_rainfall <- partial(ranger, pred.var = "critical_rainfall")
mean_critical_rainfall <- mean(data$critical_rainfall)
sd_critical_rainfall <- sd(data$critical_rainfall)
pdp_critical_rainfall$critical_rainfall <- pdp_critical_rainfall$critical_rainfall * sd_critical_rainfall + mean_critical_rainfall
ggplot(pdp_critical_rainfall, aes(x = critical_rainfall, y= yhat)) + geom_line()


plot <- test
plot$fitted <- predict(ranger, data = test)$predictions

ggplot(cer_pure, aes(x=Crop, y=Yields)) + geom_boxplot() + ggtitle('Rendement par type de culture')
ggplot(plot, aes(x=Crop, y=fitted)) + geom_boxplot() + ggtitle('Rendement estimé par type de culture')



# acp de l'ensemble des variables
acp <- PCA(X, scale.unit = F, quali.sup = c(1))
fviz_eig(acp, addlabels = TRUE)
acp$eig[1:10,]

corrplot(acp$var$cos2, is.corr=F)
corrplot(acp$var$contrib, is.corr=F)


CC1 <- acp$ind$coord[,1]
CC2 <- acp$ind$coord[,2]
CC3 <- acp$ind$coord[,3]
CC4 <- acp$ind$coord[,4]

comp_cer_pure <- data.frame(C1 = CC1, C2 = CC2, C3 = CC3, C4 =CC4)
comp_cer_pure <- cbind(cer_pure$Yields, comp_cer_pure, cer_pure$Crop)
names(comp_cer_pure)[1] <- 'Yields'
names(comp_cer_pure)[6] <- 'Crop'

#corrélation entre les variables
ggcorrplot(cor(comp_cer_pure[,-6]))


# rf composantes 
set.seed(123)
inTrain <- createDataPartition(comp_cer_pure$Yields, p=0.8, list=FALSE)
train <- comp_cer_pure[inTrain,]
test <- comp_cer_pure[-inTrain,]
ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 300,
                 mtry = 2, splitrule = 'variance', min.node.size =1)
ranger
# R2 : 0.95
# MSE : 0.07


# avec grille pour hyperparametres
# 
# hyperparam <- expand.grid(
#   mtry = 1:4,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)
# 
# final_model
# R2 : 0.949
# MSE : 0.07


# vip
graphranger <- sort(ranger$variable.importance, decreasing=TRUE)
barplot(graphranger, horiz=T,las=2,cex.names=0.6)


# pdp
pdp_C1 <- partial(ranger, pred.var = "C1")
plot(pdp_C1, type='l')
pdp_C2 <- partial(ranger, pred.var = "C2")
plot(pdp_C2, type='l')
pdp_C3 <- partial(ranger, pred.var = "C3")
plot(pdp_C3, type='l')
pdp_C4 <- partial(ranger, pred.var = "C4")
plot(pdp_C4, type='l')


# valeurs predites vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Crop = test$Crop)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Crop)) +
  scale_color_manual(values = c("green", "purple")) +
  geom_smooth(method = "lm", se = FALSE, col = "black")+
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="red")+
  labs(x = "Vraies valeurs de Yields", y = "Prédictions") +
  ggtitle("predictions vs vraies valeurs") + coord_equal()








rm(list = ls())


#### Légumineuses (culture pure et associée) ####

data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
leg <- subset(data, (Crop == 'Cowpea'))
leg <- leg[,-c(1,2,4,5)]

X_quanti <- leg[,-c(1,2)]

# Standardisation des predicteurs
variance <- function(x){sum((x-mean(x))^2)/length(x)}
standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
X <- data.frame(apply(X_quanti,2,standard))
leg <- cbind(leg$Yields, leg$Asso_pure, X)
names(leg)[c(1,2)] <- c('Yields','Asso_pure')


# rf 
set.seed(123)
inTrain <- createDataPartition(leg$Yields, p=0.8, list=FALSE)
train <- leg[inTrain,]
test <- leg[-inTrain,]

ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500,
                 mtry= 5, splitrule ='variance', min.node.size=1)
ranger
# R2 : 0.97
# MSE : 0.04


# avec grille pour hyperparametres

# 
# hyperparam <- expand.grid(
#   mtry = 1:5,
#   min.node.size = c(1, 3, 5, 10),
#   splitrule = c("extratrees", "variance")
# )
# 
# control <- trainControl(method ="cv", number = 10)
# 
# grid_search <- train(
#   Yields ~ .,
#   data = train,
#   method = "ranger",
#   trControl = control,
#   tuneGrid = hyperparam,
#   importance = "impurity",
#   num.trees= 500
# )
# 
# grid_search$bestTune
# 
# final_model <- ranger(Yields ~ ., data = train, importance = "impurity",
#                       num.trees = 500, grid_search$bestTune$mtry,
#                       min.node.size = grid_search$bestTune$min.node.size)

# final_model
# R2 : 0.97
# MSE : 0.0016


# graphique des valeurs predites vs vraies valeurs
predictions <- predict(ranger, data = test)$predictions
plot_data <- data.frame(Yields = test$Yields, Prediction = predictions, Asso_pure = test$Asso_pure)
ggplot(plot_data, aes(x= Yields, y = Prediction, color = Asso_pure)) +
  geom_point() +
  geom_abline(intercept =0, slope = 1, color="black", linewidth=1)+
  geom_smooth(method = "lm", se = FALSE, col = "purple", linewidth=1)+
  labs(x = "Vraies valeurs de Yields", y = "Predictions") + coord_equal()


# graphique importance des variables
variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") + labs(y = 'Variables') 
# Asso_pure/tot_rainfall/N_ferti/critical_rainfll


# graphiques de dépendance partielle des variables les plus importantes
pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()

pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
mean_tot_rainfall <- mean(data$tot_rainfall)
sd_tot_rainfall <- sd(data$tot_rainfall)
pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()

pdp_critical_rainfall <- partial(ranger, pred.var = "critical_rainfall")
mean_critical_rainfall <- mean(data$critical_rainfall)
sd_critical_rainfall <- sd(data$critical_rainfall)
pdp_critical_rainfall$critical_rainfall <- pdp_critical_rainfall$critical_rainfall * sd_critical_rainfall + mean_critical_rainfall
ggplot(pdp_critical_rainfall, aes(x = critical_rainfall, y= yhat)) + geom_line()


# autres graphiques de dependance partielle
pdp_nb_days_60 <- partial(ranger, pred.var = "nb_days_60")
mean_nb_days_60 <- mean(data$nb_days_60)
sd_nb_days_60 <- sd(data$nb_days_60)
pdp_nb_days_60$nb_days_60 <- pdp_nb_days_60$nb_days_60 * sd_nb_days_60 + mean_nb_days_60
ggplot(pdp_nb_days_60, aes(x = nb_days_60, y= yhat)) + geom_line()

pdp_Sowing_density <- partial(ranger, pred.var = "Sowing_density")
mean_Sowing_density <- mean(data$Sowing_density)
sd_Sowing_density <- sd(data$Sowing_density)
pdp_Sowing_density$Sowing_density <- pdp_Sowing_density$Sowing_density * sd_Sowing_density + mean_Sowing_density
ggplot(pdp_Sowing_density, aes(x = Sowing_density, y= yhat)) + geom_line()

pdp_Interrang <- partial(ranger, pred.var = "Interrang")
mean_Interrang <- mean(data$Interrang)
sd_Interrang <- sd(data$Interrang)
pdp_Interrang$Interrang <- pdp_Interrang$Interrang * sd_Interrang + mean_Interrang
ggplot(pdp_Interrang, aes(x = Interrang, y= yhat)) + geom_line()

pdp_Sowing_date <- partial(ranger, pred.var = "Sowing_date")
mean_Sowing_date <- mean(data$Sowing_date)
sd_Sowing_date <- sd(data$Sowing_date)
pdp_Sowing_date$Sowing_date <- pdp_Sowing_date$Sowing_date * sd_Sowing_date + mean_Sowing_date
ggplot(pdp_Sowing_date, aes(x = Sowing_date, y= yhat)) + geom_line()

pdp_PAWC <- partial(ranger, pred.var = "PAWC")
mean_PAWC <- mean(data$PAWC)
sd_PAWC <- sd(data$PAWC)
pdp_PAWC$PAWC <- pdp_PAWC$PAWC * sd_PAWC + mean_PAWC
ggplot(pdp_PAWC, aes(x = PAWC, y= yhat)) + geom_line()






# rm(list = ls())
#
# #### Random Forest mil ####
# 
# 
# 
# data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
# mil <- subset(data, Crop =='Millet')
# mil <- mil[,-c(1,2,4,5,8,9,10,11,12)] # retire PAWC/Sowing/SoilN/interrang (que sur un site)
# 
# 
# X_quanti <- mil[,-c(1,2)] # que les variables climat
# 
# # Standardisation des predicteurs
# variance <- function(x){sum((x-mean(x))^2)/length(x)}
# standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
# X <- data.frame(apply(X_quanti,2,standard))
# mil <- cbind(mil$Yields, mil$Asso_pure, X)
# names(mil)[c(1,2)] <- c('Yields','Asso_pure')
# 
# 
# set.seed(123)
# inTrain <- createDataPartition(mil$Yields, p=0.8, list=FALSE)
# train <- mil[inTrain,]
# test <- mil[-inTrain,]
# 
# ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500)
# ranger                 
# 
# #0.92
# 
#  
# 
# # graphique importance des variables
# 
# variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
# data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
# ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
#   geom_bar(stat = "identity") + labs(y = 'Variables') 
# 
# 
# 
# 
# 
# 
# rm(list = ls())
# 
# 
# #### Random Forest sogho ####
# 
# data <- read.csv("data/donnees.csv", sep = ";", dec = ".")
# sorgho <- subset(data, Crop =='Sorghum')
# sorgho <- sorgho[,-c(1,2,4,5)] # 
# 
# 
# X_quanti <- sorgho[,-c(1,2)]
# 
# # Standardisation des predicteurs
# variance <- function(x){sum((x-mean(x))^2)/length(x)}
# standard <- function(x){(x-mean(x))/(sqrt(variance(x)))}
# X <- data.frame(apply(X_quanti,2,standard))
# sorgho <- cbind(sorgho$Yields, sorgho$Asso_pure, X)
# names(sorgho)[c(1,2)] <- c('Yields','Asso_pure')
# 
# 
# set.seed(123)
# inTrain <- createDataPartition(sorgho$Yields, p=0.8, list=FALSE)
# train <- sorgho[inTrain,]
# test <- sorgho[-inTrain,]
# 
# ranger <- ranger(Yields~., data = train, importance = "impurity", num.trees = 500)
# ranger 
# 
# #0.90
# 
# 
# # graphique importance des variables
# 
# variable_importance <- sort(ranger$variable.importance, decreasing = TRUE)
# data_importance <- data.frame(Variable = names(variable_importance), Importance = variable_importance)
# ggplot(data_importance, aes(x = Importance, y = reorder(Variable, Importance))) +
#   geom_bar(stat = "identity") + labs(y = 'Variables') 
# 
# 
# 
# # graphique de dependance partielle des variables importantes
# 
# pdp_N_fertilization_rate <- partial(ranger, pred.var = "N_fertilization_rate")
# plot(pdp_N_fertilization_rate, type = 'l')
# mean_N_fertilization_rate <- mean(data$N_fertilization_rate)
# sd_N_fertilization_rate <- sd(data$N_fertilization_rate)
# pdp_N_fertilization_rate$N_fertilization_rate <- pdp_N_fertilization_rate$N_fertilization_rate * sd_N_fertilization_rate + mean_N_fertilization_rate
# plot(pdp_N_fertilization_rate, type = 'l')
# ggplot(pdp_N_fertilization_rate, aes(x = N_fertilization_rate, y= yhat)) + geom_line()
# 
# pdp_tot_rainfall <- partial(ranger, pred.var = "tot_rainfall")
# plot(pdp_tot_rainfall, type = 'l')
# mean_tot_rainfall <- mean(data$tot_rainfall)
# sd_tot_rainfall <- sd(data$tot_rainfall)
# pdp_tot_rainfall$tot_rainfall <- pdp_tot_rainfall$tot_rainfall * sd_tot_rainfall + mean_tot_rainfall
# plot(pdp_tot_rainfall, type = 'l')
# ggplot(pdp_tot_rainfall, aes(x = tot_rainfall, y= yhat)) + geom_line()
# 
# pdp_veg_rainfall <- partial(ranger, pred.var = "veg_rainfall")
# plot(pdp_veg_rainfall, type = 'l')
# mean_veg_rainfall <- mean(data$veg_rainfall)
# sd_veg_rainfall <- sd(data$veg_rainfall)
# pdp_veg_rainfall$veg_rainfall <- pdp_veg_rainfall$veg_rainfall * sd_veg_rainfall + mean_veg_rainfall
# plot(pdp_veg_rainfall, type = 'l')
# ggplot(pdp_veg_rainfall, aes(x = veg_rainfall, y= yhat)) + geom_line()
