options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
library(tidyverse)
library(readxl)
library(xlsx)

# Dowload des bases de donnees
# hourly_demand
hd.df <- read.csv(paste0(getwd(),'/Database/hourly_demand.csv'),sep=';', encoding = "UTF-8", check.names = F)
hd.df
# annual_demand
ad.df <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';', encoding = "UTF-8", check.names = F)
names(ad.df) <- iconv(names(ad.df), to = "ASCII", sub = "")
ad.df
# hourly_weather
hw.df <- read.csv(paste0(getwd(),'/Database/hourly_weather.csv'),sep=';', encoding = "UTF-8", check.names = F)
names(hw.df) <- iconv(names(hw.df), to = "ASCII", sub = "")

# Validations 
nrow(hourly_demand) == nrow(hourly_weather)

