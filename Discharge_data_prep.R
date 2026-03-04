#Title: Buoy data analysis 
#Author: Liz D. Ortiz Munoz
#Date: 2024/08/24
#version: R version 4.2.2
#Purpose: Prepare discharge data from Coral Gables and Little River 
#discharge was extracted from DBHYDRO - for both sites (flow instantanoues data from day 20210401-20231201)

#Coral Gables site G93 #https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8750783&v_parameter_string=&v_dbkey=64745&v_frequency=&v_sdate=19911001&v_edate=20240326&v_datum=

#Little River site S27 #https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8750772&v_parameter_string=&v_dbkey=65069&v_frequency=&v_sdate=19850531&v_edate=20240326&v_datum=

#http://my.sfwmd.gov/portal/pls/portal/realtime.pkg_rr.proc_rr?p_op=MIAMI
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

dis_cg <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/Discharge_CoralGables_20240805.xlsx")
View(dis_cg)

# dis_cg <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/BuoyChapter_20240805/Discharge_CoralGables_20240805 copy.xlsx")
# View(dis_cg)


dis_lr <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/Discharge_LittleRiver_20240805.xlsx")
View(dis_lr)

# dis_lr <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/BuoyChapter_20240805/Discharge_LittleRiver_20240805 copy.xlsx")
# View(dis_lr)

#only keep the columns that we want 
dis_cg_new<-dis_cg[,c(1,2,4)]
dis_lr_new<-dis_lr[,c(1,2,4)]

# Organizing the dates
# Split the Date column into Month, Day, and Year
dis_cg_new_2 <- dis_cg_new %>%
  separate( DBKEY , into = c("Year", "Month", "Day"), sep = "-") %>%
  separate(Day, into = c("Day", "Time"), sep = " ")%>%
  mutate(Date = as.Date(paste(Day, Month, Year, sep = "-"), format = "%d-%m-%Y"))
dis_cg_new_2
#dis_cg_new_2 <- subset(dis_cg_new_2, Date < "2023-12-02")
tail(dis_cg_new_2)

dis_lr_new_2 <- dis_lr_new %>%
  separate( DBKEY , into = c("Year", "Month", "Day"), sep = "-") %>%
  separate(Day, into = c("Day", "Time"), sep = " ")%>%
  mutate(Date = as.Date(paste(Day, Month, Year, sep = "-"), format = "%d-%m-%Y"))
dis_lr_new_2

#subset dates to match the same dates as fdom data 

dis_cg_new_2 <- dis_cg_new_2 %>%
  filter(Date >= as.Date("2021-04-21"),
         Date <= as.Date("2024-06-30"))
head(dis_cg_new_2)
tail(dis_cg_new_2)


dis_lr_new_2 <- dis_lr_new_2 %>%
  filter(Date >= as.Date("2021-04-21"),
         Date <= as.Date("2024-06-30"))
head(dis_lr_new_2)
tail(dis_lr_new_2)

#rename the stations
dis_cg_new_2$STATION <- str_replace_all(dis_cg_new_2$STATION , c("G93-S-Q" = "Coral_Gables"))
dis_lr_new_2$STATION <- str_replace_all(dis_lr_new_2$STATION , c("S27-S-Q" = "Little_River"))

## Create a new data table with averages by day
aver_daily_cg<- dis_cg_new_2 %>%
  group_by(STATION,Date, Year) %>%
  summarize(daily_ave_mean_discharge_cg = mean(UNITS_cfs, na.rm = TRUE))
aver_daily_cg
tail(aver_daily_cg)


ggplot(aver_daily_cg, aes(x= Date, y= daily_ave_mean_discharge_cg)) +
  geom_line()

aver_daily_lr<- dis_lr_new_2 %>%
  group_by(STATION,Date, Year) %>%
  summarize(daily_ave_mean_discharge_lr = mean(UNITS_cfs, na.rm = TRUE))
aver_daily_lr
tail(aver_daily_lr)
#aver_daily_lr <- aver_daily_lr[-976, ]#eliminate na row
aver_daily_lr

ggplot(aver_daily_lr, aes(x= Date, y= daily_ave_mean_discharge_lr)) +
  geom_line()

##merge the cg and lr data set
#daily data 
colnames(aver_daily_cg)[4] <- "Coral_Gables"
colnames(aver_daily_cg)
colnames(aver_daily_lr)[4] <- "Little_River"
colnames(aver_daily_lr)
merged_daily_data <- merge(aver_daily_cg, aver_daily_lr, by = c("Date", "Year"), all = TRUE)
merged_daily_data

#eliminate Station cloumns 

merged_daily_data_2<- merged_daily_data[,-c(3,5) ]
merged_daily_data_2

#daily
merged_daily_data_3 <-merged_daily_data_2 %>% 
  pivot_longer (cols = c('Coral_Gables','Little_River'),
                names_to ='Canal',
                values_to = 'Discharge')
merged_daily_data_3


# Reorder levels of the Canal factor
merged_daily_data_3$Canal <- factor(merged_daily_data_3$Canal, levels = c("Little_River", "Coral_Gables"))

merged_daily_data_3

#daily average of discharge 
discharge <- merged_daily_data_3 %>%
  mutate(Canal = factor(Canal, levels = c("Coral_Gables", "Little_River"))) %>%
  arrange(Canal, Date)

#write.csv(discharge, "combined_discharge.csv", row.names = FALSE)
