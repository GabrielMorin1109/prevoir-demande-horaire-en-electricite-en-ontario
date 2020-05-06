
library(openxlsx)
library(usethis)

# Dowload des bases de donnees
getwd()

hourly_demand <- read.csv(paste(getwd(),'/Database/hourly_demand.csv',sep=''),sep=';')
hourly_demand

annual_demand <- read.csv(paste(getwd(),'/Database/annual_demand.csv',sep=''),sep=';')
annual_demand

hourly_weather <- read.csv(paste(getwd(),'/Database/hourly_weather.csv',sep=''),sep=';')
hourly_weather

# Validations 
nrow(hourly_demand) == nrow(hourly_weather)
