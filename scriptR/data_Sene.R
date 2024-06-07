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







data_sans_climat <- read_excel("C:/Users/sinibaldi/Desktop/stage/dataframes/data_sans_climat.xlsx")






#### Senegal ####

data_Sene <- subset(data_sans_climat, Site == 'Senegal')


# liste avec l'ensemble des données climatiques du Senegal sur les 20 années

setwd("C:/Users/sinibaldi/Desktop/stage/Entrees-Sorties_stageMathilde/0-data-Simulation_plan/Senegal_Bambey")
climat_Senegal <- lapply(2000:2019, function(year) read.table(paste0("climatbambey.", year)))




tot_rainfall <- rep(NA, length(data_Sene$Cropping_situation))
veg_rainfall <- rep(NA, length(data_Sene$Cropping_situation))
rep_rainfall <- rep(NA, length(data_Sene$Cropping_situation))
critical_rainfall <- rep(NA, length(data_Sene$Cropping_situation))
nb_days_60   <- rep(NA, length(data_Sene$Cropping_situation))
nb_days_0    <- rep(NA, length(data_Sene$Cropping_situation))
max_days_0   <- rep(NA, length(data_Sene$Cropping_situation))





#### Millet pure Senegal ####



### total rainfall ###

for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' 
      & data_Sene$Crop[i] == 'Millet' 
      & data_Sene$Asso_pure[i] == 'pure'){
    
    #stocker date de debut/fin/annee
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
   
    climat_year <- climat_Senegal[[year - 2000 + 1]]  #element 1 de la liste climat 2000, element 2 climat 2001....
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}




### vegetative rainfall ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}






### reproductive rainfall ###




for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    imats <- iplts + 78
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    rep_period  <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year     <- climat_Senegal[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}









### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    climat_year <- climat_Senegal[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }

}




### max consecutive days without rainfall sur periode totale  ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'pure'){
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range   <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }

  
}













#### Millet associee Senegal ####


### total rainfall ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' 
      & data_Sene$Crop[i] == 'Millet' 
      & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###




for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}







### reproductive rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    imats <- iplts + 78
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    rep_period <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}







### critical rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 55
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}






### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    climat_year <- climat_Senegal[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###




for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Millet' & data_Sene$Asso_pure[i] == 'associee'){
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 78
    
    
    climat_year  <- climat_Senegal[[year - 2000 + 1]]
    tot_period   <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}












#### Niebe pure Senegal ####



### total rainfall ###


# tot_rainfall <- numeric(length(data_Sene$Cropping_situation))



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' 
      & data_Sene$Crop[i] == 'Cowpea' 
      & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54
    

    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    veg_period  <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}






### reproductive rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54
    imats <- iplts + 68
    

    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    rep_period  <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54
    debut <- iflos - 15
    fin   <- iflos + 15
    
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}







### nombre de jours sur la periode totale avec plus de 60mm de pluie ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    climat_year <- climat_Senegal[[year - 2000 + 1]]
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###




for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    
    year <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    climat_year <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###




for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'pure'){
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    
    climat_year  <- climat_Senegal[[year - 2000 + 1]]
    tot_period   <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}








#### Niebe associee Senegal ####



### total rainfall ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' 
      & data_Sene$Crop[i] == 'Cowpea' 
      & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    
    climat_year <- climat_Senegal[[year - 2000 + 1]] 
    tot_period  <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    
    tot_rainfall[i] <- sum(climat_year$V10[tot_period])
  }
  
  else {tot_rainfall[i] <- tot_rainfall[i]}
}






### vegetative rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54
    
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    veg_period <- climat_year$V5 >= iplts & climat_year$V5 <= iflos
    
    
    veg_rainfall[i] <- sum(climat_year$V10[veg_period])
  }
  
  else {veg_rainfall[i] <- veg_rainfall[i]}
}






### reproductive rainfall ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54 
    imats <- iplts + 68
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    rep_period <- climat_year$V5 > iflos & climat_year$V5 <= imats
    
    
    rep_rainfall[i] <- sum(climat_year$V10[rep_period])
  }
  
  else {rep_rainfall[i] <- rep_rainfall[i]}
}




### critical rainfall ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    iflos <- iplts + 54 
    debut <- iflos - 15
    fin   <- iflos + 15
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]] 
    critical_period <- climat_year$V5 >= debut & climat_year$V5 <= fin
    
    
    critical_rainfall[i] <- sum(climat_year$V10[critical_period])
  }
  
  else {critical_rainfall[i] <- critical_rainfall[i]}
}





### nombre de jours sur la periode totale avec plus de 60mm de pluie ###


for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_60[i] <- sum(climat_year$V10[tot_period] > 60)
  }
  
}






### nombre de jours sur la periode totale sans pluie ###



for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    year <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    climat_year <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    
    nb_days_0[i] <- sum(climat_year$V10[tot_period] == 0) 
  }
  
}








### max consecutive days without rainfall sur periode totale  ###





for (i in seq_along(data_Sene$Cropping_situation)){
  
  if (data_Sene$Site[i] == 'Senegal' & data_Sene$Crop[i] == 'Cowpea' & data_Sene$Asso_pure[i] == 'associee'){
    
    year  <- data_Sene$Year[i]
    iplts <- data_Sene$Sowing_date[i]
    imats <- iplts + 68
    
    
    climat_year    <- climat_Senegal[[year - 2000 + 1]]
    tot_period <- climat_year$V5 >= iplts & climat_year$V5 <= imats
    values_range   <- climat_year$V10[tot_period]
    
    
    if (all(values_range != 0)) {max_days_0[i] <- 0} 
    else {max_days_0[i] <- max(rle(values_range == 0)$lengths[rle(values_range == 0)$values == TRUE])}
  }
  
  
}






#### data final ####


data_Sene <- cbind(data_Sene, tot_rainfall, veg_rainfall, rep_rainfall, critical_rainfall,
                  nb_days_60, nb_days_0, max_days_0)





#### save as excel ####


write.xlsx(data_Sene, file="C:/Users/sinibaldi/Desktop/stage/dataframes/data_Sene.xlsx", sheetName = "general",
           colNames = TRUE, rowNames = FALSE, append= TRUE)


