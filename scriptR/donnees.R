rm(list =ls())


library(openxlsx)
library(dplyr)
library(readxl)


data_Sene <- read_excel("C:/Users/sinibaldi/Desktop/stage/dataframes/data_Sene.xlsx")
data_Burk <- read_excel("C:/Users/sinibaldi/Desktop/stage/dataframes/data_Burk.xlsx")
data_Mali <- read_excel("C:/Users/sinibaldi/Desktop/stage/dataframes/data_Mali.xlsx")

donnees <- rbind(data_Sene, data_Burk, data_Mali)





#### save as excel ####

# write.xlsx(data, file="C:/Users/sinibaldi/Desktop/stage/dataframes/donnees.xlsx", sheetName = "general",
#            colNames = TRUE, rowNames = FALSE, append= TRUE)



#### save as data R ####

# saveRDS(data, "C:/Users/sinibaldi/Desktop/stage/dataframes/donnees.rds")






