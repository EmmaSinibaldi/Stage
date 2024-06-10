#### Variables sols ####

rm(list =ls())

library(readxl)
library(dplyr)
library(openxlsx)




# PAWC (plant water available)
# PAWHC (mm) =  HCCF – HMINF
# avec HCCF (mm) = HCCF * 1/100 * DAF * epc * 10 et HMINF (mm) = HMINF * 1/100 * DAF * epc * 10
# pour chaque couche du sol.



layer_1 <- read_excel("data/Soil_layer_1_modif.xlsx")
layer_1 <- layer_1[,1:5]
layer_2 <- read_excel("data/Soil_layer_2_modif.xlsx")
layer_2 <- layer_2[,1:5]
layer_3 <- read_excel("data/Soil_layer_3_modif.xlsx")
layer_3 <- layer_3[,1:5]
layer_4 <- read_excel("data/Soil_layer_4_modif.xlsx")
layer_4 <- layer_3[,1:5]
layer_5 <- read_excel("data/Soil_layer_5_modif.xlsx")
layer_5 <- layer_3[,1:5]



layer_1 <- layer_1 %>%
  rowwise() %>% 
  mutate(PAWC1 = HCCF * 1/100 * DAF * epc * 10 - HMINF * 1/100 * DAF * epc * 10)

layer_2 <- layer_2 %>%
  rowwise() %>% 
  mutate(PAWC2 = HCCF * 1/100 * DAF * epc * 10 - HMINF * 1/100 * DAF * epc * 10)

layer_3 <- layer_3 %>%
  rowwise() %>% 
  mutate(PAWC3=HCCF * 1/100 * DAF * epc * 10 - HMINF * 1/100 * DAF * epc * 10)

layer_4 <- layer_4 %>%
  rowwise() %>% 
  mutate(PAWC4=HCCF * 1/100 * DAF * epc * 10 - HMINF * 1/100 * DAF * epc * 10)

layer_5 <- layer_5 %>%
  rowwise() %>% 
  mutate(PAWC5=HCCF * 1/100 * DAF * epc * 10 - HMINF * 1/100 * DAF * epc * 10)



layer_1_PAWC <- layer_1[,-c(2,3,4,5)]
layer_2_PAWC <- layer_2[,-c(2,3,4,5)]
layer_3_PAWC <- layer_3[,-c(2,3,4,5)]
layer_4_PAWC <- layer_4[,-c(2,3,4,5)]
layer_5_PAWC <- layer_5[,-c(2,3,4,5)]

rm(layer_1, layer_2, layer_3, layer_4, layer_5)

names(layer_1_PAWC)[1] = "soil"
names(layer_2_PAWC)[1] = "soil"
names(layer_3_PAWC)[1] = "soil"
names(layer_4_PAWC)[1] = "soil"
names(layer_5_PAWC)[1] = "soil"


df_PAWC <- merge(layer_1_PAWC, layer_2_PAWC, by="soil")
df_PAWC <- merge(df_PAWC, layer_3_PAWC, by="soil")
df_PAWC <- merge(df_PAWC, layer_4_PAWC, by="soil")
df_PAWC <- merge(df_PAWC, layer_5_PAWC, by="soil")



df_PAWC <- df_PAWC %>%
  rowwise() %>% 
  mutate(PAWC = PAWC1 + PAWC2 + PAWC3 + PAWC4 + PAWC5)

PAWC <- df_PAWC[c(1,7)]


#saveRDS(PAWC, "PAWC.rds")








# Total organic N dans le sol (norg)

soils <- read_excel("data/Soil_characteristics_modif.xlsx") 
soils_norg <- soils[c(1,3)]




#### Dataframe PAWC / Total organic N####

soils_var <- merge.data.frame(PAWC, soils_norg, by="soil")




#### save as excel ####

# write.xlsx(PAWC, file="C:/Users/sinibaldi/Desktop/stage/dataframes/PAWC.xlsx", sheetName = "general",
#            colNames = TRUE, rowNames = FALSE, append= TRUE)
# write.xlsx(soils_norg, file="C:/Users/sinibaldi/Desktop/stage/dataframes/Norg.xlsx", sheetName = "general",
#            colNames = TRUE, rowNames = FALSE, append= TRUE)
# write.xlsx(soils_var, file="C:/Users/sinibaldi/Desktop/stage/dataframes/soils_var.xlsx", sheetName = "general",
#            colNames = TRUE, rowNames = FALSE, append= TRUE)

