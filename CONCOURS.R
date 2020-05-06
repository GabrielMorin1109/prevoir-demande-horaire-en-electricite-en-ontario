options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
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
hd.df <- read.csv(paste0(getwd(),'/Database/hourly_demand.csv'),sep=';', encoding = "UTF-8")
# annual_demand
ad.df <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';', encoding = "UTF-8")
# hourly_weather
hw.df <- read.csv(paste0(getwd(),'/Database/hourly_weather.csv'),sep=';', encoding = "UTF-8")

#-----

# arrangement des dates
{
  hd.df$Date.s <- paste(hd.df$Date, hd.df$Hour, sep = " ") %>% ymd_h()
  hd.df <- hd.df[, !colnames(hd.df) %in% c("Date", "Hour", "Year", "Month")]
}


# Validations 
nrow(hourly_demand) == nrow(hourly_weather)
nrow(hourly_demand)
nrow(annual_demand)

plot(hourly_demand[hourly_demand$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
hourly_demand[hourly_demand$Total.Energy.Use.from.Electricity..MW. == min(hourly_demand[hourly_demand$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hourly_demand$Hour==1, ] 

hourly_demand[hourly_demand$Date == '15-août-03' | hourly_demand$Date == '14-août-03',]
