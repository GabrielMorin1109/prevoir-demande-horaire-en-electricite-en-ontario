# options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod",
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate",
                "opera", #package arthur
                "keras"
                )
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) {install.packages(new.packages, dependencies = T, quiet =T, repos='https://cran.rstudio.com/')}
  for(package_name in list.of.packages) {library(package_name,character.only=TRUE, quietly = TRUE)}
}
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
# DOCCUMENTATION PERTINENTE:

# proportion de la consommation en énergie par secteur: https://www.cer-rec.gc.ca/nrg/ntgrtd/mrkt/nrgsstmprfls/on-eng.html
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################

# Dowload des bases de donnees
# hourly_demand
hd.df <- read.csv(paste0(getwd(),'/Database/hourly_demand.csv'),sep=';', encoding = "UTF-8")
str(hd.df)
colnames(hd.df) <- c('Date','Hour','Load_Mw','Year','Month')
# annual_demand
ad.df <- read.csv(paste0(getwd(),'/Database/annual_demand.csv'),sep=';', encoding = "UTF-8"); ad.df$Secteur <- ad.df$Secteur %>% as.factor()
str(ad.df)
colnames(ad.df) <- c('Year','Secteur','Load_PJ','locaux','eau','electro','eclairage','refroidissement')

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

# Donnees manquantes
sapply(hd.df,function(X) sum(is.na(X))) # Aucune donnee manquante
sapply(w.df,function(X) sum(is.na(X))) # Aucune donnee manquante
sapply(ad.df,function(X) sum(is.na(X))) # Aucune donne manquante


plot(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l')
lines(hd.df[hd.df$Hour ==2,"Total.Energy.Use.from.Electricity..MW."],col='red')
hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 


hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',]

annual_demand

# TEST : Base sur cet article (https://freakonometrics.hypotheses.org/52081)
plot(hd.df[hd.df$Year == 2003,3],type='l')
model1 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year,data=hd.df[hd.df$Year == 2003,3])
summary(model1)

new_data <- hd.df[hd.df$Year == 2004,]
p = predict(model1,newdata=new_data[1:110,c(2,5,4)])
plot(new_data[1:100,3],type='l')
lines(p[1:100],col='red')
# Clairement pas great comme modele, on va ajouter la temperature

class(hd.df$Date.s)
class(w.df$Date)
w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm()

hour.df <- left_join(hd.df,w.df,by = c('Date.s'='Date.s')) # On va merge les 2 df par heure pour faciliter les modeles
hour.df <- hour.df[,-which(colnames(hour.df) =='Date.y')]

plot(x=hour.df$temperature,y=hour.df$Load_Mw)

model.2 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year + bs(temperature),data=hour.df[hour.df$Year == 2003,]) 
# j'ai mis bs pour la temperature pcq poly ne marchait pas
summary(model.2)

new_data <- hour.df[hour.df$Year == 2004,]
p = predict(model.2,newdata=new_data[1:110,c(2,5,4,8)])
plot(new_data[1:100,3],type='l')
lines(p[1:100],col='red')
# Deja beaucoup mieux

