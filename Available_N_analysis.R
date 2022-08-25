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
print(nitrogen_2022)
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
##previously made graph with Date_Collected and used as.Date to make date
class(nitrogen_2022$Days_Since_Girdle)
###output: "integer"
dates <- as.integer(nitrogen_2022$Days_Since_Girdle)


##convert character into date format yy/mm/dd
#nitrogen_2022$Date_Collected <- as.Date(nitrogen_2022$Date_Collected)
#class(nitrogen_2022$Date_Collected)

##create new data frame to make time series graph for nitrate
nitrate_time <- nitrogen_2022 %>%
  select(Days_Since_Girdle, Treatment, nitrate_calculated) %>%
  group_by(Days_Since_Girdle,Treatment) %>%
  summarise(nitrate_mean = mean(nitrate_calculated), nitrate_se = sd(nitrate_calculated)/sqrt(8))
nitrate_time

##make graph with ggplot2 for nitrate
library(ggplot2)
nitrate_graph <- ggplot(nitrate_time, aes(x=Days_Since_Girdle, y=nitrate_mean, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=nitrate_mean-nitrate_se, ymax=nitrate_mean+nitrate_se), width=.2,
                position=position_dodge(0.05))
print(nitrate_graph)
# Finished line plot
nitrate_graph+labs(title="             Soil Nitrate Over Time", x="Days Since Girdling", y = "Soil Nitrate (mg nitrate/kg soil)")+
  theme_classic() +
  scale_color_manual(values=c('#8F6B2B','#4967D7'), na.translate=F)

## save high quality plot into directory
ggsave("nitrate_time_series.jpeg", units="in", width=5, height=4, dpi=300)
getwd()

##create new data frame for ammonium time series graph
ammonium_time <- nitrogen_2022 %>%
  select(Days_Since_Girdle, Treatment, ammonium_calculated) %>%
  group_by(Days_Since_Girdle,Treatment) %>%
  summarise(ammonium_mean = mean(ammonium_calculated), ammonium_se = sd(ammonium_calculated)/sqrt(8))
ammonium_time

##make time series plot for ammonium
ammonium_graph <- ggplot(ammonium_time, aes(x=Days_Since_Girdle, y=ammonium_mean, group=Treatment, color=Treatment)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=ammonium_mean-ammonium_se, ymax=ammonium_mean+ammonium_se), width=.2,
                position=position_dodge(0.05))
print(ammonium_graph)
# Finished line plot
ammonium_graph+labs(title="             Soil Ammonium Over Time", x="Days Since Girdling", y = "Soil Ammonium (mg ammonium/kg soil)")+
  theme_classic() +
  scale_color_manual(values=c('#8F6B2B','#4967D7'), na.translate=F)

## save high quality plot into directory
ggsave("ammonium_time_series.jpeg", units="in", width=5, height=4, dpi=300)
##make sure in correct working directory
getwd()

#make box plot for ammonium
##create new data frame for boxplot, with ammonium averaged across time
# ammonium_box <- nitrogen_2022 %>%
#   select(Treatment, ammonium_calculated) %>%
#   group_by(Treatment) %>%
#   summarise(ammonium_mean = mean(ammonium_calculated), ammonium_se = sd(ammonium_calculated)/sqrt(8))
# ammonium_box
#create new data frame to omit na

nitrogen_no_blank <- nitrogen_2022 %>% drop_na()

box <- ggplot(data=nitrogen_no_blank, aes(x = Treatment, y = ammonium_calculated)) +
  geom_boxplot(fill=c('#8F6B2B','#4967D7')) +
  labs(title = '                  Soil Ammonium Across Treatment Types', 
       y= 'Soil Ammonium (mg Ammonium/kg soil)',
       x= 'Treatment Type')

print(box)
## save high quality plot into directory
ggsave("ammonium_box_plot.jpeg", units="in", width=7, height=4, dpi=300)
##make sure in correct working directory
getwd()

#test for normality with Levene's test, data are normal if p>0.01
# R program to illustrate
# Levene's test

# Import required package
library(car)
library(ggplot2)
library(ggpubr)

#make Date_Collected column as factor for statistical tests
nitrogen_2022$Date_Collected <- as.factor(nitrogen_2022$Date_Collected)
class(nitrogen_2022$Date_Collected)

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


# Build the linear model for Shapiro Wilks test, data are normal if p>0.01
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



##making nitrate normal with log, also allowing for negative values. 
nitrogen_2022$log_nitrate <- log(nitrogen_2022$nitrate_calculated - 
                                   min(nitrogen_2022$nitrate_calculated, na.rm = T)+1)


result_nitrate = leveneTest(log_nitrate ~ interaction(Date_Collected, Treatment), 
                            data = nitrogen_2022)

result_nitrate 
#p value 0.7102

##shapiro wilks test
normality_test  <- lm(log_nitrate ~ interaction(Date_Collected,Treatment),
                      data = nitrogen_2022)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
##Shapiro test of normality 
shapiro.test(residuals(normality_test))
###nitrate normal with log transformation! (p=0.07671)

##two-way ANOVA for nitrate
mean_nitrate_ANOVA <- nitrogen_2022 %>%
  group_by(Type,Date_Collected,log_nitrate) %>%
  
  # create a summary table of log_nitrate column
  summarise(
    N    = length(log_nitrate),
    mean_log_nitrate = mean(log_nitrate),
    sd   = sd(log_nitrate),
    se   = sd / sqrt(N)
  )
mean_nitrate_ANOVA

# to run the statistics we now do a statistical test on the new dataframe we created by creating a new object or model that I chose to name "height_ANOVA". The model reads like this - run a linear model (lm) where the mean_height of plants is the response variable which is a function of two independent variables, plot and native vs non-native. Use the date from the object height_mean_native_ANOVA
nitrate_ANOVA <- lm(mean_log_nitrate ~ Treatment + Date_Collected +  Treatment*Date_Collected, data = mean_nitrate_ANOVA)

# here we run the ANOVA on the object we created.
anova(nitrate_ANOVA)
nitrate_ANOVA_table <- as.data.frame(anova(nitrate_ANOVA))

####output####
# Response: mean_log_nitrate
# Df Sum Sq Mean Sq F value   Pr(>F)   
# Treatment                 1  0.349  0.3489  0.2394 0.629055   
# Date_Collected            3 30.569 10.1896  6.9934 0.001527 **
#   Treatment:Date_Collected  3  2.885  0.9616  0.6600 0.584707   
# Residuals                24 34.969  1.4570                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#ammonium normality test
ammonium_normality_test  <- lm(ammonium_calculated ~ interaction(Date_Collected,Treatment),
                      data = nitrogen_2022)

##Using leveneTest()
result_ammonium = leveneTest(ammonium_calculated ~ interaction(Date_Collected, Treatment), 
                    data = nitrogen_2022)

##print the result
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

##log transformation with ammonium
nitrogen_2022$log_ammonium <- log(nitrogen_2022$ammonium_calculated)

#levene test
result_ammonium = leveneTest(log_ammonium ~ interaction(Date_Collected, Treatment), 
                            data = nitrogen_2022)

result_ammonium 
#p value  0.5614


##shapiro wilks test
normality_test  <- lm(log_ammonium ~ interaction(Date_Collected,Treatment),
                      data = nitrogen_2022)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
##Shapiro test of normality 
shapiro.test(residuals(normality_test))
###ammonium normal with log transformation!


##two-way ANOVA for ammonium
mean_ammonium_ANOVA <- nitrogen_2022 %>%
  group_by(Treatment,Date_Collected,log_ammonium) %>%
  
  # create a summary table of log_nitrate column
  summarise(
    N    = length(log_ammonium),
    mean_log_ammonium = mean(log_ammonium),
    sd   = sd(log_ammonium),
    se   = sd / sqrt(N)
  )
mean_ammonium_ANOVA

# to run the statistics we now do a statistical test on the new dataframe we created by creating a new object or model that I chose to name "height_ANOVA". The model reads like this - run a linear model (lm) where the mean_height of plants is the response variable which is a function of two independent variables, plot and native vs non-native. Use the date from the object height_mean_native_ANOVA
ammonium_ANOVA <- lm(mean_log_ammonium ~ Treatment + Date_Collected +  Treatment*Date_Collected, data = mean_ammonium_ANOVA)

# here we run the ANOVA on the object we created.
anova(ammonium_ANOVA)
ammonium_ANOVA_table <- as.data.frame(anova(ammonium_ANOVA))

###output###
# Response: mean_log_ammonium
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# Treatment                 1  1.7178 1.71779  2.9879 0.09674 .
# Date_Collected            3  1.5509 0.51698  0.8992 0.45602  
# Treatment:Date_Collected  3  0.0336 0.01119  0.0195 0.99621  
# Residuals                24 13.7981 0.57492                  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#---------------------------------------------------------------------

#read in respiration data to eventually add to existing data frame
as_id("https://drive.google.com/drive/folders/1DTSowyhfDoIr6MhZlUacvtg_C-YonOWF") %>%
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
respiration_2022 <- read.csv("googledrive_data/Soil_respiration_ShRRED.csv", na.strings = c("NA", "na"))
print(respiration_2022)

mean_respiration_2022 <- respiration_2022 %>%
  group_by(Date,Type,Plot_.) %>%
  
  # create summary table of mean efflux
  summarise(
    Mean_efflux = mean(Efflux, na.rm = T)
  )
mean_respiration_2022

#delete rows from 2022-05-26 to 2022-06-22
mean_respiration_2022 <- mean_respiration_2022[-(1:16), , drop = FALSE]

#make date into character for upcoming function
mean_respiration_2022$Date <- as.character(mean_respiration_2022$Date)
class(mean_respiration_2022$Date)

#add a column with changing dates into week numbers
#this will be useful when comparing available N and respiration data
#since data was collected on different days

library(stringr)
#create new column for week, first starting with just having dates
mean_respiration_2022$Week = mean_respiration_2022$Date 

rep_str = c('2022-06-28'='Week 1',
            '2022-07-18'='Week 2',
            '2022-07-22'='Week 3',
            '2022-07-29'='Week 4')
mean_respiration_2022$Week <- str_replace_all(mean_respiration_2022$Week, rep_str)

#add weeks to nitrogen_2022 data set
nitrogen_2022$Week = nitrogen_2022$Date_Collected 

rep_str = c('2022-06-28'='Week 1',
            '2022-07-13'='Week 2',
            '2022-07-19'='Week 3',
            '2022-07-25'='Week 4')
nitrogen_2022$Week <- str_replace_all(nitrogen_2022$Week, rep_str)

##remove rows with NA/rows with blanks
nitrogen_no_blank <- nitrogen_2022 %>% drop_na()                                         

##change names for mean_respiration_2022 data frame
mean_respiration_2022 <- mean_respiration_2022 %>%
  rename(Treatment = Type)
mean_respiration_2022 <- mean_respiration_2022 %>%
  rename(Plot = Plot_.)
##merge data frames
total_df = merge(mean_respiration_2022, nitrogen_2022, 
            by.x=c("Plot", "Treatment", "Week"), 
            by.y=c("Plot", "Treatment", "Week"))
###"Day_since_girdle" will be incorrect for efflux data here. 
###may need to redo "Day_since_girdle" with a formula
#create scatter plot and linear regression with ammonium

#create scatterplot for regular ammonium
ammonium_scatter <- total_df %>%
  ggplot(aes(x=ammonium_calculated, y=Mean_efflux))+ 
  geom_point()+
  xlab("Ammonium (mg ammonium/kg soil)")+
  ylab("Soil Respiration (micro mol CO2/s*m^2)")+ 
  geom_smooth(method=lm)
print(ammonium_scatter)
## save high quality plot into directory
ggsave("ammonium_respiration.jpeg", units="in", width=6, height=4, dpi=300)
getwd()
#make linear model with ammonium_calculated
linear_model <- lm(Mean_efflux ~ ammonium_calculated, data = total_df)
# creates a statistical summary
summary(linear_model)
####output####
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          7.898e+00  6.836e-01  11.554 1.43e-12 ***
#   ammonium_calculated -2.891e-07  1.181e-04  -0.002    0.998    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.644 on 30 degrees of freedom
# Multiple R-squared:  1.996e-07,	Adjusted R-squared:  -0.03333 
# F-statistic: 5.989e-06 on 1 and 30 DF,  p-value: 0.9981





#make time series plot for ammonium and efflux

##create new data frame for ammonium time series graph
###this will be a separate graph for treatment
# efflux_treatment <- total_df %>%
#   select(Week, Treatment, ammonium_calculated, Mean_efflux) %>%
#   group_by(Week,Treatment) %>%
#   filter(Treatment == "Treatment") %>%
#   summarise(ammonium_mean = mean(ammonium_calculated), ammonium_se = sd(ammonium_calculated)/sqrt(8), 
#             efflux_mean = mean(Mean_efflux), efflux_se = sd(Mean_efflux)/sqrt(8))
# efflux_treatment
# 
# 
# # A few constants
# effluxColor <- "#69b3a2"
# ammoniumColor <- rgb(0.2, 0.6, 0.9, 1)
# 
# ggplot(efflux_treatment, aes(x=Week), group=1) +
#   
#   geom_line(aes(y=efflux_mean), size=2, color=effluxColor) + 
#   geom_line(aes(y=ammonium_mean), size=2, color=ammoniumColor) +
#   
#     
#   
#   scale_y_continuous(
#     
#     # Features of the first axis
#     name = "Soil Carbon Efflux",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~ . / 100, name="Ammonium (mg Ammonium/kg soil)")
#   ) + 
#   
#   theme_classic() +
#   
#   theme(
#     axis.title.y = element_text(color = effluxColor, size=13),
#     axis.title.y.right = element_text(color = ammoniumColor, size=13)
#   ) +
#   
#   ggtitle("Ammonium and Efflux Throughout Time in Treatment")
# 
# # ammonium_graph <- ggplot(ammonium_time, aes(x=Date_Collected, y=ammonium_mean, group=Treatment, color=Treatment)) + 
# #   geom_line() +
# #   geom_point()+
# #   geom_errorbar(aes(ymin=ammonium_mean-ammonium_se, ymax=ammonium_mean+ammonium_se), width=.2,
# #                 position=position_dodge(0.05))
