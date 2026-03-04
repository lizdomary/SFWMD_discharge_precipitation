#Title: Buoy data analysis 
#Author: Liz D. Ortiz Munoz
#Date: 2024/08/05
#version: R version 4.2.2
#Purpose: Prepare rainfall data from Coral Gables and Little River 
#rainfall was extracted from noaa.gov. Only one location was selected that is located near both buoys
#rainfall data shows daily max
#https://www.ncei.noaa.gov/access/past-weather/25.91879790511596,-80.5317171906139,25.670797748465127,-80.02673213919786
#MIAMI 5.7 N, FL US (US1FLMD0079)
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


# Load dplyr
library(dplyr)
library(readxl)

precipitation <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/BuoyChapter_20240805/buoy_precipitation_20240805 copy.xlsx")
View(precipitation)

# precipitation <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/buoy_precipitation_20240805.xlsx")
# View(precipitation)


met_filt <-precipitation %>% 
  dplyr:: select(Date, Precipitation_inches) %>% 
  dplyr::mutate(Date = as.Date(Date))
head(met_filt)


#selecting desired study timeframe 
met_filt2 <- met_filt %>%
  filter(Date >= as.Date("2021-04-21"),
         Date <= as.Date("2024-06-30"))
head(met_filt2)
tail(met_filt2)

#write.csv(met_filt2, "precipitation.csv", row.names = FALSE)

#######
#new precipitation that is closer to each gate 


# Load dplyr
library(dplyr)
library(readxl)

precipitation <- read_excel("/Users/lizortiz/Library/CloudStorage/OneDrive-FloridaInternationalUniversity/PhD data/Sensor data/Buoy_data/precipitation_buoys_20260107.xlsx")
#View(precipitation)
head(precipitation)

met_filt <-precipitation %>% 
  dplyr::select(Date, Precipitation_inches, Canal) %>% 
  dplyr::mutate(Date = as.Date(Date))
head(met_filt)


#selecting desired study timeframe 
met_filt2 <- met_filt %>%
  filter(Date >= as.Date("2021-04-21"),
         Date <= as.Date("2024-06-30"))
head(met_filt2)
tail(met_filt2)

#write.csv(met_filt2, "precipitation_2.csv", row.names = FALSE)
