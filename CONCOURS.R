
library(openxlsx)
library(usethis)

# Dowload des bases de donnees
getwd()

hourly_demand <- read.csv(paste0(getwd(),'/Database/hourly_demand.csv'),sep=';')
hourly_demand

annual_demand <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';')
annual_demand

hourly_weather <- read.csv(paste0(getwd(),'/Database/hourly_weather.csv'),sep=';')
hourly_weather

# Validations 
nrow(hourly_demand) == nrow(hourly_weather)
nrow(hourly_demand)
nrow(annual_demand)

plot(hourly_demand[hourly_demand$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
hourly_demand[hourly_demand$Total.Energy.Use.from.Electricity..MW. == min(hourly_demand[hourly_demand$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hourly_demand$Hour==1, ] 

hourly_demand[hourly_demand$Date == '15-août-03' | hourly_demand$Date == '14-août-03',]
