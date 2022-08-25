library(googledrive)
library(tidyverse)
library(dplyr)
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
Leaf_N <- read.csv("googledrive_data/Leaf_N_and_C.csv", na.strings = c("NA", "na"))
print(Leaf_N)

#make Type and Location columns as factors

class(Leaf_N$Type_Location)
Leaf_N$Type <- as.factor(Leaf_N$Type)
Leaf_N$Location <- as.factor(Leaf_N$Location)

#test for normality
library(car)
library(ggplot2)
library(ggpubr)

##Using leveneTest()
result = leveneTest(N_percent ~ interaction(Type,Location), 
                    data = Leaf_N)

##print the result
print(result)
# p=0.3156

##Shapiro-Wilks test
##Build the linear model for Shapiro Wilks test
normality_test  <- lm(N_percent ~ interaction(Type,Location),
                      data = Leaf_N)

##Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
##Shapiro test of normality 
shapiro.test(residuals(normality_test))
##p-value is 0.4438, so passes test

#Leaf_N t-test for tree treatment
t.test(data=Leaf_N, N_percent ~ Type)
####output####
# t = -1.9594, df = 8.8719, p-value = 0.08219
# sample estimates:
#   mean in group Control mean in group Treatment 
#                 1.96500                 2.22875 


#Leaf_N t-test for tree location
t.test(data=Leaf_N, N_percent ~ Location)
####output####
# t = 2.238, df = 3.6499, p-value = 0.09527
# mean in group Adjacent    mean in group Focal 
# 2.4275                 1.9975 



#ANOVA for leaf_N
# model <- lm( N_percent ~ Type + Location + Type*Location, data=Leaf_N )
# anova(model)
# N_ANOVA <- as.data.frame(anova(model))
# 
# interaction <- aov(N_percent ~ Type*Location, data = Leaf_N)
# 
# summary(interaction)

#not showing interaction
####output####
# Df Sum Sq Mean Sq F value Pr(>F)  
# Type         1 0.1855 0.18550   2.831 0.1268  
# Location     1 0.3160 0.31601   4.822 0.0557 .
# Residuals    9 0.5898 0.06553                 



#make a bar graph
library(ggplot2)

#create a summary table
mean_leaf_N <- Leaf_N %>%
  select(Type,Location,Type_Location,N_percent) %>%
  group_by(Type,Location) %>%
  summarise(N = length(N_percent), 
            mean_N_percent = mean(N_percent),
            sd   = sd(N_percent),
                      se   = sd / sqrt(N))
mean_leaf_N

# Basic barplot
p<-ggplot(data=mean_leaf_N, aes(x=Type_Location, y=mean_N_percent)) +
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_N_percent-se, ymax=mean_N_percent+se), width=.2,
                position=position_dodge(.9))

print(p)

p+labs(title="%N in Northern Red Oak Canopy Leaves",
       x="Tree Location", y = "% Nitrogen")+
  scale_x_discrete(labels=c("Control", "Adjacent", "Focal"))+
  theme_classic()

# # Basic barplot
# p<-ggplot(data=mean_leaf_N, aes(x=Type_Location, y=mean_N_percent)) +
#   geom_bar(stat="identity", color="black",
#            position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean_N_percent-se, ymax=mean_N_percent+se), width=.2,
#                 position=position_dodge(.9))
# 
# print(p)
# 
# p+labs(title="%N in Northern Red Oak Canopy Leaves",
#        x="Plot and Tree Location", y = "% Nitrogen")+
#   theme_classic()


