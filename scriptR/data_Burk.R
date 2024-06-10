rm(list =ls())

library(openxlsx)
library(dplyr)
library(readxl)
library(stringr)
library(rio)





###########################################################################
#                         CLIMAT                                          #
###########################################################################



# column 1: name of weather file
# column 2: year
# column 3: month
# column 4: day in month
# column 5: Julian day
# column 6: minimum temperature (°C)
# column 7: maximum temperature (°C)
# column 8: global radiation (MJ.m-2. j-1)
# column 9: Penman PET (mm.j-1)
# column 10: rainfall (mm.j-1)
# column 11: wind (m.s-1)
# column 12: vapour pressure (mbars)
# column 13: CO2 content(ppm).







data_sans_climat <- read_excel("data/data_sans_climat.xlsx")
data_sans_climat[, 6:12] <- lapply(data_sans_climat[, 6:12], as.numeric)





#### Burkina ####

data_Burk <- subset(data_sans_climat, Site == 'Burkina')


# liste avec l'ensemble des données climatiques du Burkina pour chacune des 20 années


climat_Burkina <- lapply(2000:2019, function(year) read.table(paste0("data/Burkina_Saria/Saria.", year)))






tot_rainfall <- rep(NA, length(data_Burk$Cropping_situation))
veg_rainfall <- rep(NA, length(data_Burk$Cropping_situation))
rep_rainfall <- rep(NA, length(data_Burk$Cropping_situation))
critical_rainfall <- rep(NA, length(data_Burk$Cropping_situation))
nb_days_60   <- rep(NA, length(data_Burk$Cropping_situation))
nb_days_0    <- rep(NA, length(data_Burk$Cropping_situation))
max_days_0   <- rep(NA, length(data_Burk$Cropping_situation))




# dates iflos (remplacée par idrps) et imats prises à partir des observations du Sorgho et Niebe du Mali





#### Sorgho pure Burkina ####


### total rainfall ###

for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' 
      & data_Burk$Crop[i] == 'Sorghum' 
      & data_Burk$Asso_pure[i] == 'pure'){
    
    # stocker date de debut/fin/annee
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]  
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}




### vegetative rainfall ###

for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' 
      & data_Burk$Crop[i] == 'Sorghum' 
      & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}




### reproductive rainfall ###

for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    imats <- iplts + 124
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    rep_period  <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year     <- climat_Burkina[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}









### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'pure'){
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}




### max consecutive days without rainfall sur periode totale  ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'pure'){
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range   <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}











#### Sorghum associee Burkina ####


### total rainfall ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' 
      & data_Burk$Crop[i] == 'Sorghum' 
      & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###




for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}







### reproductive rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    imats <- iplts + 124
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    rep_period <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}







### critical rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 96
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}






### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###




for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Sorghum' & data_Burk$Asso_pure[i] == 'associee'){
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 124
    
    
    climat_year  <- climat_Burkina[[year - 2000 + 1]]
    tot_period   <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}












#### Niebe pure Burkina ####



### total rainfall ###

for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' 
      & data_Burk$Crop[i] == 'Cowpea' 
      & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###

for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}






### reproductive rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    imats <- iplts + 107
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    rep_period  <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    debut <- iflos - 15
    fin   <- iflos + 15
    
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}







### nombre de jours sur la periode totale avec plus de 60mm de pluie ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###




for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    
    year <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###




for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'pure'){
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    
    climat_year  <- climat_Burkina[[year - 2000 + 1]]
    tot_period   <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}








#### Niebe associee Burkina ####



### total rainfall ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' 
      & data_Burk$Crop[i] == 'Cowpea' 
      & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    
    climat_year <- climat_Burkina[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    veg_period <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}






### reproductive rainfall ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    imats <- iplts + 107
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    rep_period <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    iflos <- iplts + 46
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}





### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###



for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    year <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    climat_year <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###





for (i in seq_along(data_Burk$Cropping_situation)){
  
  if (data_Burk$Site[i] == 'Burkina' & data_Burk$Crop[i] == 'Cowpea' & data_Burk$Asso_pure[i] == 'associee'){
    
    year  <- data_Burk$Year[i]
    iplts <- data_Burk$Sowing_date[i]
    imats <- iplts + 107
    
    
    climat_year    <- climat_Burkina[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range   <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}





#### data final ####


data_Burk <- cbind(data_Burk, tot_rainfall, veg_rainfall, rep_rainfall, critical_rainfall,
                            nb_days_60, nb_days_0, max_days_0)





#### save as excel ####

# 
# write.xlsx(data_Burk, file="C:/Users/sinibaldi/Desktop/stage/dataframes/data_Burk.xlsx", sheetName = "general",
#            colNames = TRUE, rowNames = FALSE, append= TRUE)








