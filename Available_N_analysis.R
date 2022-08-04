library(googledrive)
library(tidyverse)
library(dplyr)
##Upload Data from googledrive
# Direct Google Drive link to "ShRRED_data"
as_id("https://drive.google.com/drive/folders/1m1oaYbm4vtJtOoi9uvTl9HTnUF02GaDD") %>%
  drive_ls ->
  gdfiles
# Create a new data directory for files, if necessary
data_dir <- "googledrive_data/"
if(!dir.exists(data_dir)) dir.create(data_dir)
#Download date
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), "Downloading", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}
## Import downloaded date from new data directory "googledrive_data"
nitrogen_2022 <- read.csv("googledrive_data/Nitrogen_ShRRED_REU2022.csv", na.strings = c("NA", "na"))

#to calculate actual values for nitrate and ammonium (mg/kg soil) 

##first find gravimetric water content 
grav_water_content <- (nitrogen_2022$Wet_sample_weight_drying - nitrogen_2022$Dry_soil_weight)/nitrogen_2022$Dry_soil_weight
###add to existing data frame
nitrogen_2022$grav_water_content <- grav_water_content
###calculate nitrate and use values for statistical analysis
###calculation below for first analysis in rows 1-18, using -2.788 as blank
subset_1 <- nitrogen_2022 %>% slice(1:18)
nitrate_calculated_1 <- ((subset_1$Nitrate + 2.788)*0.025)/
  ((((subset_1$Wet_sample_weight_analysis) - 
     (subset_1$Wet_sample_weight_analysis * subset_1$grav_water_content)))/1000)
###calculation below for second analysis rows 19-36, using 1.314 as blank
subset_2 <- nitrogen_2022 %>% slice(19:36)
nitrate_calculated_2 <- ((subset_2$Nitrate - 1.314)*0.025)/
  ((((subset_2$Wet_sample_weight_analysis) - 
       (subset_2$Wet_sample_weight_analysis * subset_2$grav_water_content)))/1000)
###create data frame from nitrate and converge into single vector
nitrate <- data.frame(nitrate_calculated_1,nitrate_calculated_2)
nitrate_calculated <- as.vector(as.matrix(nitrate[,c("nitrate_calculated_1","nitrate_calculated_2")]))
nitrate_calculated
###add to original nitrogen_2022 data frame
nitrogen_2022$nitrate_calculated <- nitrate_calculated

##now for ammonium
###first analysis calculation for rows 1-18, using 57.342 blank value
ammonium_calculated_1 <- ((subset_1$Ammonium - 57.342)*0.025)/
  ((((subset_1$Wet_sample_weight_analysis) - 
       (subset_1$Wet_sample_weight_analysis * subset_1$grav_water_content)))/1000)
###second analysis calculation for rows 19-36, using 0 blank value
ammonium_calculated_2 <- ((subset_2$Ammonium - 0)*0.025)/
  ((((subset_2$Wet_sample_weight_analysis) - 
       (subset_2$Wet_sample_weight_analysis * subset_2$grav_water_content)))/1000)
###create data frame from ammonium and converge into single vector
ammonium <- data.frame(ammonium_calculated_1,ammonium_calculated_2)
ammonium_calculated <- as.vector(as.matrix(ammonium[,c("ammonium_calculated_1","ammonium_calculated_2")]))
ammonium_calculated
###add to original nitrogen_2022 data frame
nitrogen_2022$ammonium_calculated <- ammonium_calculated

#export nitrogen_2022 as csv
setwd(data_dir)
write.csv(nitrogen_2022,"CalculatedN_ShRRED_REU2022.csv", row.names = FALSE)
#for some reason, data is going to my PC, not google drive?

