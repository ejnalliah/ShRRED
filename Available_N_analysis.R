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



#making time series graphs of available N

##view column class 
class(nitrogen_2022$Date_Collected)
###output: "character"
dates <- as.Date(nitrogen_2022$Date_Collected)
class(dates)
##convert character into date format yy/mm/dd
nitrogen_2022$Date_Collected <- as.Date(nitrogen_2022$Date_Collected)

##create new data frame to make time series graph for nitrate
nitrate_time <- nitrogen_2022 %>%
  select(Date_Collected, Treatment, nitrate_calculated) %>%
  group_by(Date_Collected,Treatment) %>%
  summarise(nitrate_mean = mean(nitrate_calculated), nitrate_se = sd(nitrate_calculated)/sqrt(8))
nitrate_time

##make graph with ggplot2 for nitrate
library(ggplot2)
nitrate_graph <- ggplot(nitrate_time, aes(x=Date_Collected, y=nitrate_mean, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=nitrate_mean-nitrate_se, ymax=nitrate_mean+nitrate_se), width=.2,
                position=position_dodge(0.05))
print(nitrate_graph)
# Finished line plot
nitrate_graph+labs(title="             Soil Nitrate Over Time", x="Date Sampled", y = "Soil Nitrate (mg nitrate/kg soil)")+
  theme_classic() +
  scale_color_manual(values=c('#8F6B2B','#4967D7'))

## save high quality plot into directory
ggsave("nitrate_time_series.jpeg", units="in", width=5, height=4, dpi=300)
getwd()

##create new data frame for ammonium time series graph
ammonium_time <- nitrogen_2022 %>%
  select(Date_Collected, Treatment, ammonium_calculated) %>%
  group_by(Date_Collected,Treatment) %>%
  summarise(ammonium_mean = mean(ammonium_calculated), ammonium_se = sd(ammonium_calculated)/sqrt(8))
ammonium_time

##make time series plot for ammonium
ammonium_graph <- ggplot(ammonium_time, aes(x=Date_Collected, y=ammonium_mean, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=ammonium_mean-ammonium_se, ymax=ammonium_mean+ammonium_se), width=.2,
                position=position_dodge(0.05))
print(ammonium_graph)
# Finished line plot
ammonium_graph+labs(title="             Soil Ammonium Over Time", x="Date Sampled", y = "Soil Ammonium (mg ammonium/kg soil)")+
  theme_classic() +
  scale_color_manual(values=c('#8F6B2B','#4967D7'))

## save high quality plot into directory
ggsave("ammonium_time_series.jpeg", units="in", width=5, height=4, dpi=300)
##make sure in correct working directory
getwd()


#test for normality with Levene's test
# R program to illustrate
# Levene’s test

# Import required package
library(car)
library(ggplot2)
library(ggpubr)
# Using leveneTest()
result = leveneTest(nitrate_calculated ~ interaction(Date_Collected, Treatment), 
                    data = nitrogen_2022)

# print the result
print(result)

####Output####
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  7  0.5415 0.7945
#       24 



##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(nitrate_calculated ~ interaction(Date_Collected,Treatment),
                      data = nitrogen_2022)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro.test(residuals(normality_test))
####output####
#Shapiro-Wilk normality test

#data:  residuals(normality_test)
#W = 0.87636, p-value = 0.001633

#since p<0.01, data are significantly different from normal distribution
#look at website for more info
##http://www.sthda.com/english/wiki/normality-test-in-r


#ammonium normality test
ammonium_normality_test  <- lm(ammonium_calculated ~ interaction(Date_Collected,Treatment),
                      data = nitrogen_2022)

# Using leveneTest()
result_ammonium = leveneTest(ammonium_calculated ~ interaction(Date_Collected, Treatment), 
                    data = nitrogen_2022)

# print the result
print(result_ammonium)
####output####
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  7  0.6591 0.7038
#       24 

# Create a QQ plot of residuals
ggqqplot(residuals(ammonium_normality_test))
# Shapiro test of normality 
shapiro.test(residuals(ammonium_normality_test))
####output####
#Shapiro-Wilk normality test

#data:  residuals(ammonium_normality_test)
#W = 0.81834, p-value = 9.194e-05


