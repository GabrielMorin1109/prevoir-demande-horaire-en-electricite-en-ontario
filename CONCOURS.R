options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
library(tidyverse)
library(lubridate)
library(stringr)
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod",
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) {install.packages(new.packages, dependencies = T, quiet =T, repos='https://cran.rstudio.com/')}
  for(package_name in list.of.packages) {library(package_name,character.only=TRUE, quietly = TRUE)}
}
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
# Dowload des bases de donnees
# hourly_demand
hd.df <- read.csv(paste0(getwd(),'/Database/hourly_demand.csv'),sep=';', encoding = "UTF-8")#, check.names = F)
hd.df
# annual_demand
ad.df <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';', encoding = "UTF-8")#, check.names = F)
iconv(names(ad.df), to = "Latin1")
names(ad.df) <- iconv(names(ad.df), to = "ASCII//TRANSLIT")#, sub = "")
names(ad.df)
ad.df
# hourly_weather
hw.df <- read.csv(paste0(getwd(),'/Database/hourly_weather.csv'),sep=';', encoding = "UTF-8")#, check.names = F)
names(hw.df) <- iconv(names(hw.df), to = "ASCII", sub = "")

# Validations 
nrow(hourly_demand) == nrow(hourly_weather)

