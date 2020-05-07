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



# as.numeric(gsub(",", "", y))

# arrangement des variables numerique
{ # pour demand
  ad.df$Secteur <- as.character(levels(ad.df$Secteur))[ad.df$Secteur] # on enleve la variable secteur de type levels, car elle ne doit pas etre considerer dans le apply.
  ad.df[,ad.df %>% dplyr::select_if(is.factor) %>% colnames] <-  # correction des variables numeriques avec des "," en var num avec des "."
    apply(ad.df %>% dplyr::select_if(is.factor), c(1,2), function(my.df){
      as.numeric(gsub(",", ".", my.df))
    })
  ad.df$Secteur <- as.factor(ad.df$Secteur) # retour de la variable a une variable factorielle
}

{ # de meme pour le weather
  w.df$Date <- as.character(levels(w.df$Date))[w.df$Date]
  w.df[,w.df %>% dplyr::select_if(is.factor) %>% colnames] <-  # correction des variables numeriques avec des "," en var num avec des "."
    apply(w.df %>% dplyr::select_if(is.factor), c(1,2), function(my.df){
      as.numeric(gsub(",", ".", my.df))
    })
  w.df$Date <- as.factor(w.df$Date) # retour de la variable a une variable factorielle
}
# as.numeric(levels(ad.df[,"Consommation.electricite.totale..PJ."]))[ad.df[,"Consommation.electricite.totale..PJ."]]


<<<<<<< HEAD

as.character(levels(ad.df$Secteur))[ad.df$Secteur]
=======
# as.character(levels(ad.df$Secteur))[ad.df$Secteur]
>>>>>>> 4398aa63bc17b1374cd27510340243178401d5aa
# Validations 
nrow(hd.df) == nrow(w.df)
nrow(hd.df)
nrow(ad.df)



plot(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
lines(hd.df[hd.df$Hour ==2,"Total.Energy.Use.from.Electricity..MW."],col='red')
hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 


hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',]

annual_demand
