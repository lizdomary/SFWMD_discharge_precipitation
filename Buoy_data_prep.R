#Title: Buoy data analysis 
#Author: Liz D. Ortiz Munoz
#Date: 2024/08/05
#version: R version 4.2.2
#Purpose:Prepare Buoy data from Coral Gables and Little River

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

#upload data

data <- read.csv("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/Buoydata_allyears_20240805.csv")

# data <- read.csv("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/BuoyChapter_20240805/Buoydata_allyears_20240805 copy.csv")

data<- data %>% mutate(date = mdy(Date))

#================================================================================================
#Coral gables data preparation 
#================================================================================================
coral<-subset(data, Canal== "Coral_Gables")
#data check
hist(coral$FDOM_QSU)
mean(coral$FDOM_QSU, na.rm = TRUE)
median(coral$FDOM_QSU, na.rm = TRUE)

sd_coral <- sd(coral$FDOM_QSU, na.rm = TRUE)

#running QAQC and removing values greater than 2 S.D. from previous and subsequent values (Quality assurance and quality control)
QAQC_coral <- coral %>% 
  mutate(current = lag(FDOM_QSU, 0),
         before = lag(FDOM_QSU, 1),
         after = lead(FDOM_QSU, 1)) %>% 
  mutate(Fdom_qsu = ifelse(
    ( abs(before - current) > (2*sd_coral)   )  & ( abs(after - current) > (2*sd_coral)   ), NA, FDOM_QSU
  )) #%>% 
# select(-current, -before, -after, -Fdom_qsu)
head(QAQC_coral)

sd(QAQC_coral$fdom_qsu, na.rm = TRUE)

#determining number of NAs in dataset 
exo_na_coral <- QAQC_coral %>%
  filter(is.na(Fdom_qsu))
head(exo_na_coral)


#plot(exo_filta$DateTime, exo_filta$EXO_fdom_qsu)
mean(QAQC_coral$Fdom_qsu, na.rm = TRUE)
median(QAQC_coral$Fdom_qsu, na.rm = TRUE)
sd(QAQC_coral$Fdom_qsu, na.rm = TRUE)
hist(QAQC_coral$Fdom_qsu)

#name coral  
coral<-QAQC_coral
# Filter the data to keep only rows with dates after 2021-04-21
filtered_data_CG <- subset(coral, Date2 > as.Date("2021-04-20")) # subset the data after 2021-04-20 because the first point on LR is shown at 2021-04-21

#================================================================================================
#little river data preparation 
#================================================================================================

little<-subset(data, Canal== "Little_River")
#data check
hist(little$FDOM_QSU)
mean(little$FDOM_QSU, na.rm = TRUE)
median(little$FDOM_QSU, na.rm = TRUE)

sd_little <- sd(little$FDOM_QSU, na.rm = TRUE)

#running QAQC and removing values greater than 2 S.D. from previous and subsequent values (Quality assurance and quality control)
QAQC_little <- little %>% 
  mutate(current = lag(FDOM_QSU, 0),
         before = lag(FDOM_QSU, 1),
         after = lead(FDOM_QSU, 1)) %>% 
  mutate(Fdom_qsu = ifelse(
    ( abs(before - current) > (2*sd_little)   )  & ( abs(after - current) > (2*sd_little)   ), NA, FDOM_QSU
  )) #%>% 
#select(-current, -before, -after, -Fdom_qsu)
head(QAQC_little)

sd(QAQC_little$fdom_qsu, na.rm = TRUE)

#determining number of NAs in dataset 
exo_na_little <- QAQC_little %>%
  filter(is.na(Fdom_qsu))
head(exo_na_little)


#plot(exo_filta$DateTime, exo_filta$EXO_fdom_qsu)
mean(QAQC_little$Fdom_qsu, na.rm = TRUE)
median(QAQC_little$Fdom_qsu, na.rm = TRUE)
sd(QAQC_little$Fdom_qsu, na.rm = TRUE)
hist(QAQC_little$Fdom_qsu)

#name coral  
little<-QAQC_little

#================================================================================================
#Create daily and monthly values and data sets
#================================================================================================

#Coral gables 
#create daily lag values for coral gables 
exo_daily <- filtered_data_CG %>%
  group_by(date) %>%
  summarize(
    daily_EXO_fdom = mean(Fdom_qsu, na.rm = TRUE),
    daily_EXO_spCond = mean(spCond_uS_cm, na.rm = TRUE),
    daily_EXO_do_mgL = mean(ODO_Concplus_mg_L, na.rm = TRUE),
    daily_EXO_sal_psu = mean(Sal_psu, na.rm = TRUE),
    daily_EXO_depth = mean(Depth_meters, na.rm = TRUE),
    daily_EXO_pH = mean(pH, na.rm = TRUE),
    daily_EXO_chla_ugL = mean(Chlorophyll_ug_L, na.rm = TRUE), 
    daily_EXO_Turbidity_NTU = mean(Turbidity_NTU, na.rm = TRUE))


head(exo_daily)
tail(exo_daily)

day_fin_coral <- exo_daily %>% 
  mutate(daily_EXO_spCond_ZT = as.numeric(scale(daily_EXO_spCond, center = TRUE, scale = TRUE)),
         daily_EXO_do_mgL_ZT = as.numeric(scale(daily_EXO_do_mgL , center = TRUE, scale = TRUE)),
         daily_EXO_sal_psu_ZT = as.numeric(scale(daily_EXO_sal_psu, center = TRUE, scale = TRUE)),
         daily_EXO_pH_ZT = as.numeric(scale( daily_EXO_pH, center = TRUE, scale = TRUE)),
         daily_EXO_chla_ugL_ZT = as.numeric(scale(daily_EXO_chla_ugL, center = TRUE, scale = TRUE)),
         daily_EXO_Turbidity_NTU_ZT = as.numeric(scale(daily_EXO_Turbidity_NTU, center = TRUE, scale = TRUE)))
#mutate gets ztransformed data for each driver 

head(day_fin_coral)
tail(day_fin_coral)

day_fin_coral[is.na(day_fin_coral)] <- NA  #sets NaN from inflow days with no data to NA  
#write.csv(day_fin_coral, "ar_daily_data_joined_RAINLAGS_coral.csv", row.names = FALSE)

#daily value Little River
#create daily lag values for little river 
exo_daily_lt <- little %>%
  group_by(date) %>%
  summarize(
    daily_EXO_fdom = mean(Fdom_qsu, na.rm = TRUE),
    daily_EXO_spCond = mean(spCond_uS_cm, na.rm = TRUE),
    daily_EXO_do_mgL = mean(ODO_Concplus_mg_L, na.rm = TRUE),
    daily_EXO_sal_psu = mean(Sal_psu, na.rm = TRUE),
    daily_EXO_depth = mean(Depth_meters, na.rm = TRUE),
    daily_EXO_pH = mean(pH, na.rm = TRUE),
    daily_EXO_chla_ugL = mean(Chlorophyll_ug_L, na.rm = TRUE), 
    daily_EXO_Turbidity_NTU = mean(Turbidity_NTU, na.rm = TRUE))


head(exo_daily_lt)
tail(exo_daily_lt)

day_fin_little <- exo_daily_lt %>% 
  mutate(daily_EXO_spCond_ZT = as.numeric(scale(daily_EXO_spCond, center = TRUE, scale = TRUE)),
         daily_EXO_do_mgL_ZT = as.numeric(scale(daily_EXO_do_mgL , center = TRUE, scale = TRUE)),
         daily_EXO_sal_psu_ZT = as.numeric(scale(daily_EXO_sal_psu, center = TRUE, scale = TRUE)),
         daily_EXO_pH_ZT = as.numeric(scale( daily_EXO_pH, center = TRUE, scale = TRUE)),
         daily_EXO_chla_ugL_ZT = as.numeric(scale(daily_EXO_chla_ugL, center = TRUE, scale = TRUE)),
         daily_EXO_Turbidity_NTU_ZT = as.numeric(scale(daily_EXO_Turbidity_NTU, center = TRUE, scale = TRUE)))
#mutate gets ztransformed data for each driver 

head(day_fin_little)
tail(day_fin_little)

day_fin_little[is.na(day_fin_little)] <- NA  #sets NaN from inflow days with no data to NA  
#write.csv(day_fin_coral, "ar_daily_data_joined_RAINLAGS_coral.csv", row.names = FALSE)

#####Monthly data set ######
#coral gables 
exo_monthly_coral <- filtered_data_CG %>% 
  group_by(yr = year(date), mon = month(date))%>% 
  summarize(monthly_EXO_fdom = mean(Fdom_qsu, na.rm = TRUE),
            monthly_EXO_spCond = mean(spCond_uS_cm, na.rm = TRUE),
            monthly_EXO_do_mgL = mean(ODO_Concplus_mg_L , na.rm = TRUE),
            monthly_EXO_sal_psu = mean(Sal_psu, na.rm = TRUE),
            monthly_EXO_depth = mean(Depth_meters, na.rm = TRUE),
            monthly_EXO_pH = mean(pH, na.rm = TRUE), 
            monthly_EXO_chla_ugL = mean(Chlorophyll_ug_L, na.rm = TRUE),
            monthly_EXO_Turbidity_NTU = mean(Turbidity_NTU, na.rm = TRUE)) %>% 
  mutate(day = sum(0+1)) %>% #creates a day value just to create a date function
  mutate(Date = as.Date( paste(yr, mon, day,sep="-"), "%Y-%m-%d")) %>% 
  mutate(monthly_EXOchla_ugL_ZT= as.numeric(scale(monthly_EXO_chla_ugL, center = TRUE, scale = TRUE)),
         monthly_EXO_do_mgL_ZT= as.numeric(scale(monthly_EXO_do_mgL, center = TRUE, scale = TRUE)),
         monthly_EXO_spCond_ZT= as.numeric(scale(monthly_EXO_spCond, center = TRUE, scale = TRUE)),
         monthly_EXO_sal_psu_ZT= as.numeric(scale( monthly_EXO_sal_psu, center = TRUE, scale = TRUE)),
         monthly_EXO_pH_ZT= as.numeric(scale(monthly_EXO_pH, center = TRUE, scale = TRUE)),
         monthly_EXO_Turbidity_NTU_ZT= as.numeric(scale(monthly_EXO_Turbidity_NTU, center = TRUE, scale = TRUE)))


head(exo_monthly_coral)
tail(exo_monthly_coral)
exo_monthly_final_coral <- exo_monthly_coral %>% 
  select(Date, 
         monthly_EXO_fdom, 
         monthly_EXO_chla_ugL, 
         monthly_EXOchla_ugL_ZT, 
         monthly_EXO_do_mgL, 
         monthly_EXO_do_mgL_ZT,
         monthly_EXO_spCond,
         monthly_EXO_spCond_ZT,
         monthly_EXO_sal_psu,
         monthly_EXO_sal_psu_ZT,
         monthly_EXO_pH,
         monthly_EXO_pH_ZT,
         monthly_EXO_Turbidity_NTU,
         monthly_EXO_Turbidity_NTU_ZT, 
         monthly_EXO_depth, 
         yr, 
         mon)

# #exo_monthly_final_coral <- exo_monthly_coral %>% 
#   select(Date, 
#          monthly_EXO_fdom , 
#          monthly_EXO_chla_ugL, 
#          monthly_EXOchla_ugL_ZT, 
#          monthly_EXO_do_mgL, 
#          monthly_EXO_do_mgL_ZT,
#          monthly_EXO_spCond,
#          monthly_EXO_spCond_ZT,
#          monthly_EXO_sal_psu,
#          monthly_EXO_sal_psu_ZT,
#          monthly_EXO_pH,
#          monthly_EXO_pH_ZT,
#          monthly_EXO_Turbidity_NTU,
#          monthly_EXO_Turbidity_NTU_ZT, 
#          monthly_EXO_depth, 
#          yr, 
#          mon)

# Check the result
#print(exo_monthly_final_coral)


#Little River

exo_monthly_little <- little %>% 
  group_by(yr = year(date), mon = month(date))%>% 
  summarize(monthly_EXO_fdom = mean(Fdom_qsu, na.rm = TRUE),
            monthly_EXO_spCond = mean(spCond_uS_cm, na.rm = TRUE),
            monthly_EXO_do_mgL = mean(ODO_Concplus_mg_L , na.rm = TRUE),
            monthly_EXO_sal_psu = mean(Sal_psu, na.rm = TRUE),
            monthly_EXO_depth = mean(Depth_meters, na.rm = TRUE),
            monthly_EXO_pH = mean(pH, na.rm = TRUE), 
            monthly_EXO_chla_ugL = mean(Chlorophyll_ug_L, na.rm = TRUE),
            monthly_EXO_Turbidity_NTU = mean(Turbidity_NTU, na.rm = TRUE)) %>% 
  mutate(day = sum(0+1)) %>% #creates a day value just to create a date function
  mutate(Date = as.Date( paste(yr, mon, day,sep="-"), "%Y-%m-%d")) %>% 
  mutate(monthly_EXOchla_ugL_ZT = as.numeric(scale(monthly_EXO_chla_ugL, center = TRUE, scale = TRUE)),
         monthly_EXO_do_mgL_ZT = as.numeric(scale(monthly_EXO_do_mgL, center = TRUE, scale = TRUE)),
         monthly_EXO_spCond_ZT = as.numeric(scale(monthly_EXO_spCond, center = TRUE, scale = TRUE)),
         monthly_EXO_sal_psu_ZT = as.numeric(scale( monthly_EXO_sal_psu, center = TRUE, scale = TRUE)),
         monthly_EXO_pH_ZT = as.numeric(scale(monthly_EXO_pH, center = TRUE, scale = TRUE)),
         monthly_EXO_Turbidity_NTU_ZT = as.numeric(scale(monthly_EXO_Turbidity_NTU, center = TRUE, scale = TRUE)))

head(exo_monthly_little)
tail(exo_monthly_little)

# exo_monthly_final_little  <- exo_monthly_little %>% 
#   select(Date, monthly_EXO_fdom, monthly_EXO_chla_ugL, monthly_EXOchla_ugL_ZT, monthly_EXO_do_mgL, monthly_EXO_do_mgL_ZT,
#          monthly_EXO_chla_ugL, monthly_EXOchla_ugL_ZT,monthly_EXO_spCond,monthly_EXO_spCond_ZT,monthly_EXO_sal_psu,
#          monthly_EXO_sal_psu_ZT,monthly_EXO_pH,monthly_EXO_pH_ZT,monthly_EXO_Turbidity_NTU, monthly_EXO_Turbidity_NTU_ZT,
#          monthly_EXO_depth, yr, mon)  #have to keep yr and mon since their currently grouping variables. can filter out during AR analysis after reading in new csv

#================================================================================================
#join monthly and daily data set from both sites
#================================================================================================

#Create new names for canals
day_fin_coral$Canal<-"Coral_Gables"
day_fin_little$Canal<-"Little_River"

exo_monthly_coral$Canal<-"Coral_Gables"
exo_monthly_little$Canal<-"Little_River"

#exo_monthly_final_coral$Canal<-"Coral_Gables"
#exo_monthly_final_little$Canal<-"Little_River"


# Assuming both data frames have the same columns join and save data sets

combined_data_daily <- rbind(day_fin_coral, day_fin_little)
#write.csv(combined_data_daily, "combined_data_daily.csv", row.names = FALSE)

combined_data_monthly <- rbind(exo_monthly_coral, exo_monthly_little)
#write.csv(combined_data_monthly, "combined_data_monthly.csv", row.names = FALSE)

#combined_data_monthly_final <- rbind(exo_monthly_final_coral, exo_monthly_final_little)
