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
str(hd.df)
# annual_demand
ad.df <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';', encoding = "UTF-8"); ad.df$Secteur <- ad.df$Secteur %>% as.factor()
str(ad.df)

# hourly_weather
w.df <- read.csv(paste0(getwd(),'/Database/hourly_weather.csv'),sep=';', encoding = "UTF-8")
str(w.df)
#-----

# arrangement des dates
hd.df$Date.s <- paste(hd.df$Date, hd.df$Hour, sep = " ") %>% ymd_h()
str(hd.df)

# arrangement des variables numerique
{ # pour demand
  ad.df[,3:ncol(ad.df)] <-  # correction des variables numeriques avec des "," en var num avec des "."
    sapply(ad.df[,3:ncol(ad.df)], function(my.df){as.numeric(gsub(",", ".", my.df))})
}

{ # de meme pour le weather
  w.df[,2:ncol(w.df)] <-  # correction des variables numeriques avec des "," en var num avec des "."
    sapply(w.df[,2:ncol(w.df)], function(my.df){as.numeric(gsub(",", ".", my.df))})
}
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Validations 
nrow(hd.df) == nrow(w.df)
nrow(hd.df)
nrow(ad.df)



plot(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
lines(hd.df[hd.df$Hour ==2,"Total.Energy.Use.from.Electricity..MW."],col='red')
hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 


hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',]

annual_demand


