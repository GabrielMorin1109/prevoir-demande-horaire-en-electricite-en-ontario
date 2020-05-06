options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
library(tidyverse)


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
nrow(hd.df) == nrow(hw.df)
nrow(hd.df)
nrow(ad.df)

plot(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 

hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',]

annual_demand
