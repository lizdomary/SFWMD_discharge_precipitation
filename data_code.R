#Title: Buoy data analysis 
#Author: Liz D. Ortiz Munoz
#Date: 2025/01/07
#version: R version 4.2.2
#Purpose: create time seria linear models with both Coral Gables and Little River
#undestand fdom and discharge data
#discharge was extracted from DBHYDRO - for both sites (flow instantanoues data from day 20210401-20231201)

#Coral Gables site G93 #https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8750783&v_parameter_string=&v_dbkey=64745&v_frequency=&v_sdate=19911001&v_edate=20240326&v_datum=

#Little River site S27 #https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8750772&v_parameter_string=&v_dbkey=65069&v_frequency=&v_sdate=19850531&v_edate=20240326&v_datum=

#http://my.sfwmd.gov/portal/pls/portal/realtime.pkg_rr.proc_rr?p_op=MIAMI
dev.off()
#================================================================================================
#library
#install.packages("astsa")
library(astsa) 
pacman::p_load(tidyverse, cowplot, lubridate, dplyr, dplR, zoo)
library(naniar)
#install.packages("segmented")
library(DescTools) #lets you plot the Timeseries, ACF and PACF in one window using PlotACF function 
library(WaveletComp)
library(dplyr)
library(matrixStats)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(viridis)

library(car)
library(devtools)
library(ggpubr) # for the facet
library(ggplot2) # Used to create the visualizations
library(tidyverse) # Used for data tidying
library(dplyr) # Used for data tidying
library(janitor) # Used to make all column names in the same format
library(lubridate) # Used to deal with dates
library(neonOS) # Functions for common data wrangling needs for NEON observational data
library(terra) # Spatial data package; needed for remote sensing data
library(imputeTS) # Deal with NA's when doing Time Series
library(tidyr) #pivot tables
library(corrplot) #correlation matrix
library(segmented)
library(rcompanion)
#================================================================================================
# If you did not start RStudio from the RStudio project file found in the module
# Zip file, set your working directory to the location where you have placed the
# data files
# setwd("my_directory_path_here")
setwd("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/SFWMD_discharge_precipitation")

#================================================================================================

#load data sets 

#merge buoy_data and discharge (only variables that I want)
new_data <- read.csv("new_data.csv")
head(new_data)
tail(new_data)

#merge new_data (buoy_data and discharge) with precipitation  
new_data_prep <- read.csv("new_data_prep.csv")
head(new_data_prep)
tail(new_data_prep)


#merge all of the variables of buoy data with discharge and precipitation 
new_data_all_final <- read.csv("new_data_all_final.csv")
head(new_data_all_final)
tail(new_data_all_final)

#================================================================================================
#================================================================================================
#adjust data sets
# #add wet and dry season to data set
new_data_all_final_2 <- new_data_all_final %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", remove = FALSE) %>%
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Day = as.character(Day))

new_data_all_final_2 


new_data_all_final_2$Seasons <- ifelse(grepl("5|6|7|8|9|10", new_data_all_final_2$Month, ignore.case = TRUE), "Wet", "Dry")
new_data_all_final_2

new_data_all_final<-new_data_all_final_2

new_data_all_final$Q = new_data_all_final$Discharge * 0.028316846592

#remove dates that have bad temperatures/conductivity values 
new_data_all_final$Date <- as.Date(new_data_all_final$Date)  # make sure it's in Date format

# Remove rows between 2021-11-11 and 2021-11-23
new_data_all_final_filtered <- new_data_all_final %>%
  filter(!(Date >= as.Date("2021-11-11") & Date <= as.Date("2021-11-23")))

#remove ph outliers
new_data_all_final_filtered_2 <- new_data_all_final_filtered %>%
  mutate(daily_EXO_pH = ifelse(daily_EXO_pH < 5 | daily_EXO_pH > 10, NA, daily_EXO_pH))



#subset canals
new_data_2<-new_data_all_final_filtered_2

write.csv(new_data_2, "new_data_2_20260216.csv", row.names = FALSE)

new_data_2_coral <- subset(new_data_all_final_filtered_2, Canal == "Coral_Gables") 
new_data_2_river <- subset(new_data_all_final_filtered_2, Canal == "Little_River") 
dat2=subset(new_data_all_final_filtered_2, Canal== "Little_River")
dat=subset(new_data_all_final_filtered_2, Canal== "Coral_Gables")


#PCA Little River ####

library("factoextra")
head(dat2)
colnames(dat2)
bio_variables_little <- dat2[, c("daily_EXO_fdom","daily_EXO_spCond","daily_EXO_do_mgL", "daily_EXO_sal_psu","daily_EXO_pH","daily_EXO_chla_ugL", "Q","daily_EXO_Turbidity_NTU", "Year", "Seasons","Month")]
bio_variables_little$Month<-as.character(bio_variables_little$Month)
head(bio_variables_little)
bio_variables_little<- na.omit(bio_variables_little)


bio_variables_little <- bio_variables_little %>%
  dplyr:: rename(
    fDOM= daily_EXO_fdom,
    Conductivity = daily_EXO_spCond,
    DO=daily_EXO_do_mgL,
    Salinity= daily_EXO_sal_psu,
    pH=daily_EXO_pH, 
    Chlorophyll_a = daily_EXO_chla_ugL, 
    Discharge= Q)

library("FactoMineR")
library("factoextra")
iris.pca_little <- PCA(bio_variables_little[, c(1:8)], scale = TRUE, graph = FALSE)


fviz_pca_biplot(iris.pca_little,
                #geom.ind = "point", # show points only (nbut not "text")
                col.ind = bio_variables_little$Seasons, # color by groups
                #palette = c("#00AFBB", "#E7B800", "yellow"),
                palette = "jco",label = "var",
                col.var = "black", repel = TRUE,
                addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups")


#extract dimmesnsion 1 and 2
scores_little <- as.data.frame(iris.pca_little$ind$coord)

bio_variables_little$PC1 <- scores_little$Dim.1
bio_variables_little$PC2 <- scores_little$Dim.2

pca1_box_little<-ggplot(bio_variables_little, aes(y=PC1, x= Seasons, color = Seasons ))  + geom_violin()+
  geom_boxplot(width=0.1) +labs(
    x = "",
    y = "PC1",
    color = "Seasons",
  ) +geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_minimal() 


pca2_box_little<-ggplot(bio_variables_little, aes(y=PC2, x= Seasons, color = Seasons))  + geom_violin()+
  geom_boxplot(width=0.1) +labs(
    x = "",
    y = "PC2",
    color = "Seasons",
  ) +geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_minimal() 

pca_box_little <- ggarrange(pca1_box_little,pca2_box_little,
                            ncol = 2,
                            common.legend = TRUE #,
                            #legend = "none"
)

pca_box_little


kruskal.test(PC1 ~ Seasons, data = bio_variables_little)
# Perform Dunn's test
library(FSA)
dunn_results <- dunnTest(PC1 ~ Seasons, data= bio_variables_little, method = "bonferroni")

# Extract p-values
p_table <- dunn_results$res

# Generate compact letter display (CLD) based on adjusted p-values
# First create a summary of pairwise comparisons
cld <- cldList(P.adj ~ Comparison, data = p_table, threshold = 0.05)

# View letters
print(cld)

kruskal.test(PC2 ~ Seasons, data = bio_variables_little)
# Perform Dunn's test
library(FSA)
dunn_results <- dunnTest(PC2 ~Seasons, data= bio_variables_little, method = "bonferroni")

# Extract p-values
p_table <- dunn_results$res

# Generate compact letter display (CLD) based on adjusted p-values
# First create a summary of pairwise comparisons
cld <- cldList(P.adj ~ Comparison, data = p_table, threshold = 0.05)

# View letters
print(cld)

#PCA Coral Gables ####

library("factoextra")
head(dat)
colnames(dat)
bio_variables <- dat[, c("daily_EXO_fdom","daily_EXO_spCond","daily_EXO_do_mgL", "daily_EXO_sal_psu","daily_EXO_pH","daily_EXO_chla_ugL", "Q","daily_EXO_Turbidity_NTU", "Year","Seasons","Month")]

bio_variables$Month<-as.character(bio_variables$Month)
head(bio_variables)
bio_variables<- na.omit(bio_variables)


bio_variables <- bio_variables %>%
  dplyr:: rename(
    fDOM= daily_EXO_fdom,
    Conductivity = daily_EXO_spCond,
    DO=daily_EXO_do_mgL,
    Salinity= daily_EXO_sal_psu,
    pH=daily_EXO_pH, 
    Chlorophyll_a = daily_EXO_chla_ugL, 
    Discharge= Q)

library("FactoMineR")
library("factoextra")
iris.pca <- PCA(bio_variables[, c(1:8)], scale = TRUE, graph = FALSE)

fviz_pca_biplot(iris.pca,
                #geom.ind = "point", # show points only (nbut not "text")
                col.ind = bio_variables$Seasons, # color by groups
                #palette = c("#00AFBB", "#E7B800", "yellow"),
                palette = "jco",label = "var",
                col.var = "black", repel = TRUE,
                addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups")


#extract dimmesnsion 1 and 2
scores <- as.data.frame(iris.pca$ind$coord)

bio_variables$PC1 <- scores$Dim.1
bio_variables$PC2 <- scores$Dim.2

pca1_box_coral<-ggplot(bio_variables, aes(y=PC1, x=Seasons, color = Seasons ))  + geom_violin()+
  geom_boxplot(width=0.1) +labs(
    x = "",
    y = "PC1",
    color = "Seasons",
  ) +geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_minimal() 


pca2_box_coral<-ggplot(bio_variables, aes(y=PC2, x=Seasons, color = Seasons ))  + geom_violin()+
  geom_boxplot(width=0.1) +labs(
    x = "",
    y = "PC2",
    color = "Seasons",
    #title = "Little River: fDOM vs. Discharge by Cluster"
  ) +geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_minimal() 

pca_box_coral <- ggarrange(pca1_box_coral,pca2_box_coral,
                           ncol = 2,
                           common.legend = TRUE #,
                           #legend = "none"
)
pca_box_coral


kruskal.test(PC1 ~Seasons, data = bio_variables)
# Perform Dunn's test
library(FSA)
dunn_results <- dunnTest(PC1 ~ Seasons, data= bio_variables, method = "bonferroni")

# Extract p-values
p_table <- dunn_results$res

# Generate compact letter display (CLD) based on adjusted p-values
# First create a summary of pairwise comparisons
cld <- cldList(P.adj ~ Comparison, data = p_table, threshold = 0.05)

# View letters
print(cld)

kruskal.test(PC2 ~ Seasons, data = bio_variables)
# Perform Dunn's test
library(FSA)
dunn_results <- dunnTest(PC2 ~ Seasons, data= bio_variables, method = "bonferroni")

# Extract p-values
p_table <- dunn_results$res

# Generate compact letter display (CLD) based on adjusted p-values
# First create a summary of pairwise comparisons
cld <- cldList(P.adj ~ Comparison, data = p_table, threshold = 0.05)

# View letters
print(cld)
                           