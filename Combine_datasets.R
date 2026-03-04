#Title: Buoy data analysis 
#Author: Liz D. Ortiz Munoz
#Date: 2024/08/24
#version: R version 4.2.2
#Purpose: Combine buoy data, discharge and precipitation in the same data set 

#================================================================================================
#library
library(astsa) 
pacman::p_load(tidyverse, cowplot, lubridate, dplyr, dplR, zoo)
library(naniar)
#install.packages("DescTools")
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
library(readxl)
#================================================================================================

#load data sets 

#Buoy data- daily means 
buoy_data <- read.csv("combined_data_daily.csv")
head(buoy_data)
tail(buoy_data)

buoy_data <- read.csv("combined_data_daily.csv")
head(buoy_data)
tail(buoy_data)

#Discharge data- daily means 
discharge_data <- read.csv("combined_discharge.csv")
head(discharge_data)
tail(discharge_data)


#Precipitation data- daily max
# precipitation_data <- read.csv("precipitation.csv")
# head(precipitation_data)
# tail(precipitation_data)

#Precipitation data- daily max
precipitation_data <- read.csv("precipitation_2.csv")
head(precipitation_data)
tail(precipitation_data)

#================================================================================================
#merge buoy_data and discharge (only variables that I want)
#================================================================================================

#make new data set with the columns I cant to merge from fdom
combine<-buoy_data[,c(1,16,2,3,5) ]
combine
head(combine)
tail(combine)
colnames(combine)[1] <- "Date"
head(combine)

#merge when the canal and the date are the same
new_data <- merge(discharge_data, combine, by = c("Date", "Canal"), all = TRUE)
new_data
head(new_data)
tail(new_data)

#arrange the order of canals
new_data<- new_data %>%
  mutate(Canal = factor(Canal, levels = c("Coral_Gables", "Little_River"))) %>%
  arrange(Canal, Date)
head(new_data)
tail(new_data)

#write.csv(new_data, "new_data.csv", row.names = FALSE)

#================================================================================================
#merge new_data (buoy_data and discharge) with precipitation 
#================================================================================================

new_data_prep <- merge(new_data, precipitation_data, by = c("Date", "Canal"), all = TRUE)
new_data_prep
head(new_data_prep)
tail(new_data_prep)


#arrange the order of canals
new_data_prep<- new_data_prep %>%
  mutate(Canal = factor(Canal, levels = c("Coral_Gables", "Little_River"))) %>%
  arrange(Canal, Date)
head(new_data_prep)
tail(new_data_prep)

#write.csv(new_data_prep, "new_data_prep.csv", row.names = FALSE)

#================================================================================================
#merge all of the variables of buoy data with discharge and precipitation 
#================================================================================================

#merge when the canal and the date are the same
colnames(buoy_data)[1] <- "Date"
new_data_all <- merge(discharge_data, buoy_data, by = c("Date", "Canal"), all = TRUE)
new_data_all
head(new_data_all)
tail(new_data_all)

#merge precipitation 
new_data_prep_all <- merge(new_data_all, precipitation_data, by = c("Date", "Canal"), all = TRUE)
new_data_prep_all
head(new_data_prep_all)
tail(new_data_prep_all)

#arrange the order of canals
new_data_all_final<- new_data_prep_all %>%
  mutate(Canal = factor(Canal, levels = c("Coral_Gables", "Little_River"))) %>%
  arrange(Canal, Date)
head(new_data_all_final)
tail(new_data_all_final)

#write.csv(new_data_all_final, "new_data_all_final.csv", row.names = FALSE)
