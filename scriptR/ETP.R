


rm(list =ls())

library(openxlsx)
library(dplyr)
library(readxl)
library(stringr)
library(rio)




# noms des colonnes des fichiers climat

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







#### Burkina ####


liste_climat_Burkina <- lapply(2000:2019, function(year) read.table(paste0("data/Burkina_Saria/Saria.", year)))
climat_Burkina <- do.call(rbind, liste_climat_Burkina)
names(climat_Burkina) = c("name_of_weather_file", "year", "month", "day_in_month", "Julian_day","minimum_temperature",
                          "maximum_temperature", "global_radiation", "Penman_PET",
                          "rainfall", "wind","vapour_pressure","CO2_content")


HR_Burkina <- read.csv("data/Burkina_Saria/BurkinaHR.CSV", sep=";")
HR_Burkina <- HR_Burkina[-(1:10),]
names(HR_Burkina) <- c('Year', 'MO', 'DY', 'HR')


climat_Burkina <- climat_Burkina %>% mutate(merge_indic = 1:nrow(climat_Burkina))
HR_Burkina <- HR_Burkina %>% mutate(merge_indic = 1:nrow(HR_Burkina))


climat_Burkina <- merge(climat_Burkina, HR_Burkina, 'merge_indic')




# Calculs des differents parametres


# Temperature moyenne (°C)
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(moy_temperature = (minimum_temperature + maximum_temperature) / 2)


# Rgnet : rayonnement global net (MJm-2.day-1)
alpha <-  0.23 # albedo, constante
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(Rgnet = (1 - alpha ) * global_radiation)


# es : pression de vapeur saturée à température moyenne (kPa) 
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(es = 0.6108 * exp((17.27 * moy_temperature) / (237.3 + moy_temperature)))


# ea : pression de vapeur effective (kPA)
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(ea = as.numeric(es) * as.numeric(HR) * 1/100)


# P : pression atmospherique (kPa) z :altitude (m), constante = 1m
z <- 300
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(P = 101.3 * ((293 - 0.0065 * z) / 293)^5.26)


# gamma : constante psychomztrique (kPa.°C-1)
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(gamma = 0.665e-3 * P)


# delta : pente de la courbe de pression de vapeur saturée (kPa.°C_1)
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(delta = (4098 * es) / ((237.3 + moy_temperature))^2)





#### ETP journaliere avec formule de Penman ####


# ETP Penman (mm.day-1)
climat_Burkina <- climat_Burkina %>%
  rowwise() %>% 
  mutate(ETP = (0.408 * delta * Rgnet + gamma * 900/(moy_temperature + 273) *
                  wind * (es - ea)) / (delta + gamma * (1 + 0.34 * wind)))




#### ETP annuelle ####

# ETP annuelle 
ETP_annuelle <- list()
for (annee in 2000:2019){
  data_annee <- climat_Burkina[climat_Burkina$year == annee,]
  ETP <- sum(data_annee$ETP)
  ETP_annuelle[[as.character(annee)]] <- ETP
}


data_ETP_annuelle <- data.frame(ETP = unlist(ETP_annuelle))





### ETP sur la periode de culture ####


# liste climat
liste <- list()
for (annee in 2000:2019){
  data_annee <- climat_Burkina[climat_Burkina$year == annee,]
  liste [[as.character(annee)]]<- data_annee
}


# etp niebe
liste_etp_niebe <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 212 & liste[[i]]$Julian_day <= (212+107)
  liste_etp_niebe[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}
data_etp_niebe <- data.frame(annee = 2000:2019, ETP_niebe = unlist(liste_etp_niebe))



# etp sorgho
liste_etp_sorgho <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 197 & liste[[i]]$Julian_day <= (197+124)
  liste_etp_sorgho[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}
data_etp_sorgho <- data.frame(annee = 2000:2019, ETP_sorgho = unlist(liste_etp_sorgho))


# etp Burkina
data_etp_Burkina <- merge(data_etp_niebe, data_etp_sorgho, 'annee')












rm(list =ls())


#### Mali ####


liste_climat_Mali <- lapply(1991:2010, function(year) read.table(paste0("data/Mali_Ntarla/Ntarla.", year)))
liste_climat_Mali[[12]] <- (liste_climat_Mali[[12]])[1:364,] # pb annee 2002
climat_Mali <- do.call(rbind, liste_climat_Mali)
names(climat_Mali) = c("name_of_weather_file", "year", "month", "day_in_month", "Julian_day","minimum_temperature",
                       "maximum_temperature", "global_radiation", "Penman_PET",
                       "rainfall", "wind","vapour_pressure","CO2_content")


HR_Mali <- read.csv("data/Mali_Ntarla/MaliHR.CSV", sep=";")
HR_Mali <- HR_Mali[-(1:10),]
names(HR_Mali) <- c('Year', 'MO', 'DY', 'HR')


# manque 20 valeurs car jour julian V5 commence a 2 chaque annee
# (a retirer de HR_Mali)

HR_Mali <- subset(HR_Mali, !(MO == '1' & DY == '1'))

climat_Mali <- climat_Mali %>% mutate(merge_indic = 1:nrow(climat_Mali))
HR_Mali <- HR_Mali %>% mutate(merge_indic = 1:nrow(HR_Mali))


climat_Mali <- merge(climat_Mali, HR_Mali, 'merge_indic')





# Calculs des differents parametres


# Temperature moyenne (°C)
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(moy_temperature = (minimum_temperature + maximum_temperature) /2)


# Rgnet : rayonnement global net (MJm-2.day-1)
alpha <-  0.23 #albedo, constante
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(Rgnet = (1 - alpha ) * global_radiation)


# es : pression de vapeur saturée à température moyenne (kPa) 
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(es = 0.6108 * exp((17.27 * moy_temperature) / (237.3 + moy_temperature)))


# ea : pression de vapeur effective (kPA)
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(ea = as.numeric(es) * as.numeric(HR) * 1/100)


# P : pression atmospherique (kPa) z :altitude (m), constante = 1m
z <- 321
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(P = 101.3 * ((293 - 0.0065 * z) / 293)^5.26)


# gamma : constante psychometrique (kPa.°C-1)
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(gamma = 0.665e-3 * P)


# delta : pente de la courbe de pression de vapeur saturée (kPa.°C_1)
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(delta = (4098 * es) / ((237.3 + moy_temperature))^2)




#### ETP journaliere avec formule de Penman ####

# ETP Penman (mm.day-1)
climat_Mali <- climat_Mali %>%
  rowwise() %>% 
  mutate(ETP = (0.408 * delta * Rgnet + gamma * 900/(moy_temperature + 273) *
                  wind * (es - ea)) / (delta + gamma * (1 + 0.34 * wind)))




#### ETP annuelle  ####


ETP_annuelle <- list()
for (annee in 1991:2010){
  data_annee <- climat_Mali[climat_Mali$year == annee,]
  ETP <- sum(data_annee$ETP)
  ETP_annuelle[[as.character(annee)]] <- ETP
}


data_ETP_annuelle <- data.frame(ETP = unlist(ETP_annuelle))






### ETP sur la periode de culture ####

# liste climat par annee
liste <- list()
for (annee in 1991:2010){
  data_annee <- climat_Mali[climat_Mali$year == annee,]
  liste [[as.character(annee)]]<- data_annee
}


# etp niebe
liste_etp_niebe <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 192 & liste[[i]]$Julian_day <= (192+107)
  liste_etp_niebe[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}

data_etp_niebe <- data.frame(annee = 1991:2010, ETP_niebe = unlist(liste_etp_niebe))



# etp sorgho
liste_etp_sorgho <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 177 & liste[[i]]$Julian_day <= (177+124)
  liste_etp_sorgho[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}


data_etp_sorgho <- data.frame(annee = 1991:2010, ETP_sorgho = unlist(liste_etp_sorgho))


data_etp_Mali <- merge(data_etp_niebe, data_etp_sorgho, 'annee' )












rm(list =ls())


#### Senegal ####


liste_climat_Senegal <- lapply(2000:2019, function(year) read.table(paste0("data/Senegal_Bambey/climatbambey.", year)))
climat_Senegal <- do.call(rbind, liste_climat_Senegal)
names(climat_Senegal) = c("name_of_weather_file", "year", "month", "day_in_month", "Julian_day","minimum_temperature",
                          "maximum_temperature", "global_radiation", "Penman_PET",
                          "rainfall", "wind","vapour_pressure","CO2_content")


HR_Senegal <- read.csv("data/Senegal_Bambey/SeneHR.csv", sep=";")
HR_Senegal <- HR_Senegal[-(1:10),]
names(HR_Senegal) <- c('Year', 'MO', 'DY', 'HR')


# 21 observations manquantes dans climat_Senegal car jours julian (V5) commencent a 2 chaque annee
# (premier jour de chaque annee a retirer dans HR_Senegal) et en 2019  à 3


HR_Senegal <- subset(HR_Senegal, !(MO == '1' & DY == '1'))
HR_Senegal <- subset(HR_Senegal, !(Year=='2019' & MO == '1' & DY == '2'))

climat_Senegal <- climat_Senegal %>% mutate(merge_indic = 1:nrow(climat_Senegal))
HR_Senegal <- HR_Senegal %>% mutate(merge_indic = 1:nrow(HR_Senegal))


climat_Senegal <- merge(climat_Senegal, HR_Senegal, 'merge_indic')





# Calculs des differents parametres


# Temperature moyenne (°C)
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(moy_temperature = (minimum_temperature + maximum_temperature) /2)


# Rgnet : rayonnement global net (MJ.m-2.day-1)
alpha <-  0.23 # albedo, constante
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(Rgnet = (1 - alpha ) * global_radiation)


# es : pression de vapeur saturée à température moyenne (kPa) 
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(es = 0.6108 * exp((17.27 * moy_temperature) / (237.3 + moy_temperature)))


# ea : pression de vapeur effective (kPA)
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(ea = as.numeric(es) * as.numeric(HR) * 1/100)


# P : pression atmospherique (kPa) z :altitude (m), constante = 1m
z <- 22
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(P = 101.3 * ((293 - 0.0065 * z) / 293)^5.26)


# gamma : constante psychométrique (kPa.°C-1)
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(gamma = 0.665e-3 * P)


# delta : pente de la courbe de pression de vapeur saturée (kPa.°C_1)
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(delta = (4098 * es) / ((237.3 + moy_temperature)^2))




#### ETP avec formule de Penman  ####

# ETP Penman (mm.day-1)
climat_Senegal <- climat_Senegal %>%
  rowwise() %>% 
  mutate(ETP = (0.408 * delta * Rgnet + gamma * (900 / (moy_temperature + 273)) *
                  wind * (es - ea)) / (delta + gamma * (1 + 0.34 * wind)))



### ETP annuelle ####

# Calcul ETP annuelle avec ETP Penman calculée
ETP_calculee <- list()
for (annee in 2000:2019){
  data_annee <- climat_Senegal[climat_Senegal$year == annee,]
  ETP <- sum(data_annee$ETP)
  ETP_calculee[[as.character(annee)]] <- ETP
}


data_ETP_calculee <- data.frame(ETP = unlist(ETP_calculee))



# Calcul ETP annuelle avec ETP des données climat
ETP_donnees <- list()
for (annee in 2000:2019){
  data_annee <- climat_Senegal[climat_Senegal$year == annee,]
  ETP <- sum(data_annee$Penman_PET)
  ETP_donnees[[as.character(annee)]] <- ETP
}


data_ETP_donnees <- data.frame(ETP = unlist(ETP_donnees))




# ETP annuelle calculée / ETP annuelle data climat

ETP <- cbind(data_ETP_calculee, data_ETP_donnees)
names(ETP) <- c('ETP_calculee', 'ETP_donnees')




### ETP sur la periode de culture ####

# liste climat par annee
liste <- list()
for (annee in 2000:2019){
  data_annee <- climat_Senegal[climat_Senegal$year == annee,]
  liste [[as.character(annee)]]<- data_annee
}



# etp niebe avec donnees du data climat
liste_etp_niebe <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 246 & liste[[i]]$Julian_day <= (246+68)
  liste_etp_niebe[i] <- sum(liste[[i]]$Penman_PET[periode_culture])
  
}


data_etp_niebe <- data.frame(annee = 2000:2019, ETP_niebe_data = unlist(liste_etp_niebe))


# etp niebe avec etp calculee 
liste_etp2_niebe <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 246 & liste[[i]]$Julian_day <= (246+68)
  liste_etp2_niebe[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}

data_etp2_niebe <- data.frame(annee = 2000:2019, ETP_niebe_calculee = unlist(liste_etp2_niebe))

# etp niebe
etp_niebe <- merge(data_etp_niebe, data_etp2_niebe, by='annee')





# etp mil avec etp des donnees data climat
liste_etp_mil <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 230 & liste[[i]]$Julian_day <= (230+78)
  liste_etp_mil[i] <- sum(liste[[i]]$Penman_PET[periode_culture])
  
}

data_etp_mil <- data.frame(annee = 2000:2019, ETP__mil_data = unlist(liste_etp_mil))


# etp mil avec etp calculee
liste_etp2_mil <- list()
for (i in 1:length(liste)){
  
  periode_culture <- liste[[i]]$Julian_day >= 230 & liste[[i]]$Julian_day <= (230+78)
  liste_etp2_mil[i] <- sum(liste[[i]]$ETP[periode_culture])
  
}
data_etp2_mil <- data.frame(annee = 2000:2019, ETP_mil_calculee = unlist(liste_etp2_mil))

# etp mil
etp_mil <- merge(data_etp_mil, data_etp2_mil, 'annee')



# etp Senegal
data_etp <- merge(etp_mil, etp_niebe, 'annee')
 


