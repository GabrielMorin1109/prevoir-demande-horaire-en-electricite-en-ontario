# options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library ----
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod", "gam", 
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate", "purrr", "data.table", "bestglm", 'dplyr',
                "xts", "forecast", "tseries", # for time series object
                "corrgram",'nlme', #correlation gram, semblable a acf
                "opera", #package arthur
                # "keras", # info sur son utilisation: https://www.datacamp.com/community/tutorials/keras-r-deep-learning
                # "tensorflow",
                'tree', 'randomForest', # pour faire des arbres
                'doParallel', "foreach", "parallel",
                'timeDate',
                'bestglm',
                'chron',
                'hutilscpp'#, #cumsum_reset
                # 'RQuantLib'
                )

  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) {install.packages(new.packages, dependencies = T, quiet =T, repos='https://cran.rstudio.com/')}
  for(package_name in list.of.packages) {library(package_name,character.only=TRUE, quietly = TRUE)}
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOCCUMENTATION PERTINENTE:

# proportion de la consommation en énergie par secteur: https://www.cer-rec.gc.ca/nrg/ntgrtd/mrkt/nrgsstmprfls/on-eng.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dowload des bases de donnees ----
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

# Sunshine
#getwd()
#sun.df <- fromJSON(file = paste0(getwd(),'/Database/sunshine.json'))
#print(sun.df)
#sun.df$result$spatial

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#w.df[c(which(duplicated(w.df$Date))-1,which(duplicated(w.df$Date))),]


# arrangement des dates
hd.df$Date.s <- paste(hd.df$Date, hd.df$Hour, sep = " ") %>% paste0(., ":00") %>% ymd_hm()
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Arrangement des doublons 

{ # Pour la valid
  w.dup.df <- w.df[c(which(duplicated(w.df$Date))-1,which(duplicated(w.df$Date))),]
  w.dup.df <- w.dup.df[order(c(which(duplicated(w.df$Date))-1,which(duplicated(w.df$Date)))),]
} 

w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm()
# which(is.na(w.df$Date.s))
w.df <- w.df[,!colnames(w.df) %in% "Date"] # pour ne pas creer de confusion entre les bases de donnees
w.df <- aggregate(w.df, by = list(w.df$Date.s), mean)

identical(length(w.df$Date.s), length(unique(w.df$Date.s))) # All work!!

{
  mean(w.dup.df[c(1,2),2]) == w.df[as.numeric(rownames(w.dup.df))[1],2] # OK
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MERGE des bases de donnees weather et demande aux heures 
#  on remarque que les bases de donnees ont des differences:
list(w.df.dif = which(!(hd.df$Date.s %in% w.df$Date.s)) %>% hd.df$Date.s[.],
     hd.df.dif = which(!(w.df$Date.s %in% hd.df$Date.s)) %>% w.df$Date.s[.])
# Ainsi, on fait l'union des deux bases de donnees avec all=T —— idk si left_join ne le faisait pas, pour verifier : identical(merge(hd.df, w.df, by = "Date.s", all=T), left_join(hd.df,w.df,by = 'Date.s'))
hour.I.df <- merge(hd.df, w.df, by = "Date.s", all.x=T)  # On va merge les 2 df par heure pour faciliter les modeles
which(is.na(hour.I.df)) %>% hour.I.df[.,]
# On retire les donnees ou Load_Mw est NA
{
  identical(length(unique(hd.df$Date.s)),nrow(hd.df))
  identical(length(unique(w.df$Date.s)),nrow(w.df))
  nrow(hd.df) - nrow(w.df) # Je m'attend a 14 NA dans le merge
  length(which(is.na(hour.I.df$temperature))) # En effet, il y a 14 NA dans la temperature, enlevons ces lignes (la 15e ligne ca doit etre a cause des heures qui fit pas) 
}

hour.I.df <- hour.I.df[which(!is.na(hour.I.df$temperature)),]
sum(is.na(hour.I.df))

hour.I.df <- na.omit(hour.I.df[!is.na(hour.I.df$Load_Mw),!colnames(hour.I.df) %in% c("Date",
                                                                                     "Group.1")])
# hour.I.df$Date.s <- force_tz(hour.I.df$Date.s,"America/Toronto")

# ###
# hour_Mw.ts <- ts(hour.I.df$Load_Mw,
#               start= c(2003,1,1),#decimal_date(ymd_hms("2003-01-01 01:00:00")), #hour.I.df[1,'Date.s']
#               end = c(2016,12,31),#decimal_date(ymd_hms("2016-12-31 23:00:00")), # hour.I.df[nrow(hour.I.df),'Date.s']),
#               frequency=24)
# time(hour_Mw.ts)
# hour_Mw.stl <- stl(hour_Mw.ts, s.window = "period")
# # adf.test(diff(hour_Mw.ts), alternative="stationary", k=0) # on rejette l'hypothese null que la tim serie est stationnaire
# 
# 
# plot(hour_Mw.stl)  # top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
# monthplot(hour_Mw.stl, choice = "seasonal")  # variation in milk production for each month
# seasonplot(hour_Mw.ts)
# # fit <- arima(hour_Mw.ts, order=c(p, d, q))
# 
# hour.xts <- xts(hour.I.df, order.by = hour.I.df$Date.s)
# HoltWinters(hour_Mw.ts, beta=FALSE, gamma = FALSE) %>% plot()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Proportion de la consommation d'electricite par le Residentiel 
{
  ad.ls <- split(ad.df, ad.df$Year)
  sum.year.conso <- purrr::map(ad.ls, ~.x$Load_PJ %>% sum)
  prop.conso.ls <- 
    purrr::map2(ad.ls, sum.year.conso,
                ~sapply(1:nrow(.x),function(i){
                  .x$Load_PJ[i]/.y
                }) %>% 
                  {cbind(.x, proportion.conso = .)}
    )
  ad.p.df <- prop.conso.ls %>% reduce(rbind)
}
{
  hour.ls <- hour.I.df %>% split(year(hour.I.df$Date.s))
  hour.df <- lapply(names(hour.ls), function(my.year){
    tmp <- hour.ls[[my.year]]
    tmp$Load_Mw <- 
      tmp$Load_Mw * ad.p.df[ad.p.df$Secteur == "Residentiel",] %>% 
      {.[.$Year == as.numeric(my.year),!colnames(.) %in% c("Year", "Secteur")]} %>%  #/ nrow(hour.ls[[my.year]])}
      {.[,"proportion.conso"]}
    tmp
  }) %>% reduce(bind_rows)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# On va utiliser 70% des donnees pour le training : ----
train <- 1:(ceiling(0.7*nrow(hour.df)))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###
hour.ts <-  ts(hour.df,
               start= c(2003,1,1),#decimal_date(ymd_hms("2003-01-01 01:00:00")), #hour.I.df[1,'Date.s']
               end = c(2016,12,31), #decimal_date(ymd_hms("2016-12-31 23:00:00")), # hour.I.df[nrow(hour.I.df),'Date.s']),
               frequency=24*365/12) # saison = 1 mois ici
cycle(hour.ts)
hour.year.ts <- ts(hour.df,
                   start= c(2003,1,1),
                   end = c(2016,12,31),
                   frequency=24*365)
time(hour.ts)
hour_Mw.stl <- stl(hour.ts[,"Load_Mw"], s.window = "period")
# hour.ts.multi <- decompose(hour.ts,type = "mult")
hour_Mw.ts <- ts(hour.df$Load_Mw, start= c(2003,1,1), end = c(2016,12,31), frequency=24*365/12)
hour_Mw.ts %>% decompose() %>% plot
hour.year.ts %>% decompose %>% plot
hour_Mw.ts %>% decompose(type = "multiplicative") %>% plot
# plot(hour.ts.multi)
# adf.test(diff(hour_Mw.ts), alternative="stationary", k=0) # on rejette l'hypothese null que la tim serie est stationnaire

plot(hour.year.ts[,"Load_Mw"])
plot(hour_Mw.stl)  # top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
monthplot(hour_Mw.stl, choice = "seasonal") 

seasonplot(ts(hour.df$Load_Mw, start= c(2003,1,1), end = c(2016,12,31), frequency=24), year.labels=TRUE)
seasonplot(ts(hour.df$Load_Mw, start= c(2003,1,1), end = c(2016,12,31), frequency=24*7), year.labels = TRUE)

plot(hour_Mw.stl)
plot(stl(hour.year.ts[,"Load_Mw"], s.window = "periodic")) # decroissance de la consommation p/r aux annees
# hour.xts <- xts(hour.df, order.by = hour.df$Date.s)
HoltWinters(hour_Mw.ts, beta=FALSE, gamma = FALSE) %>% plot()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualisation d'une serie
acf(hour.df$Load_Mw, lag=24) #autocorrelation par 24h
acf(hour.df$Load_Mw, lag=24*7)
acf(hour.year.ts[,"Load_Mw"],lag = 24*365)
acf(hour.ts[,"Load_Mw"])
pacf(hour_Mw.ts)
acf(hour_Mw.ts)
# auto.arima(hour_Mw.ts)
decompose(hour_Mw.ts, type = "multiplicative")$random %>% na.omit %>% acf
corrgram(hour.ts)
# on remarque que l'ossiation est constante dans les annees. Ainsi, on peut predire sur tout les annees a partir d'aujourd'hui : https://www.r-bloggers.com/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

# Mesure s'il y a une constance dans la serie (aka, une non croissance p/r au temps)
# plot(lm(Load_Mw~Date.s, data = hour.df))# %>% summary()
{plot(hour.df$Date.s, hour.df$Load_Mw,pch='.')
  lm.fit <- lm(Load_Mw~Date.s, data = hour.df)
  x.interval <- seq(min(hour.df$Date.s), max(hour.df$Date.s), by = "hour") 
  abline(lm.fit, col = "red")
  legend('topright', legend = paste0(c("pente : ", as.character(lm.fit$coefficients["Date.s"]))), bty = 'n')
  lm.fit %>% summary()}


# Donc, decroissance relativement faible ici, mais significative.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLEAN.df ----
clean.df <- hour.df

# On ajoute le jour du mois
clean.df$Day <- day(clean.df$Date.s)

# On ajoute le jour de la semaine
clean.df$weekday <- wday(clean.df$Date.s)

# On ajoute la moyenne de consommation par jour de la semaine par heure
{ 
  mean_by_wday.df <- with(clean.df,aggregate(Load_Mw,by=list(weekday,Hour),FUN=mean))
  colnames(mean_by_wday.df) <- c('weekday','Hour','mean_by_wday_Mw')
  mean_by_wday.df$ID_mean_by_wday <- paste(mean_by_wday.df$Hour,mean_by_wday.df$weekday,sep='-')
  clean.df$ID_mean_by_wday <- paste(clean.df$Hour,clean.df$weekday,sep='-')
  clean.df <- left_join(clean.df,mean_by_wday.df,by='ID_mean_by_wday',suffix=c('','.y'))
  clean.df <- clean.df[,-which(colnames(clean.df) %in% c('ID_mean_by_wday','weekday.y','Hour.y', "Load_Mw.y"))]
}

# On ajoute la moyenne de consommation par jour de l'annee par heure
{
  clean.df$year_day <- yday(clean.df$Date.s)
  mean_by_year.df <- with(clean.df,aggregate(Load_Mw,by=list(year_day),FUN=mean))
  colnames(mean_by_year.df) <- c('year_day','mean_by_yearday_Mw')
  clean.df <- left_join(clean.df,mean_by_year.df,by='year_day')
}

# weekend ----
{
  clean.df$Weekend <- rep(0, nrow(clean.df))
  clean.df$Weekend[which(clean.df$weekday %in% c(1,7))] <- 1
}


# Temperature ----
{
  #clean.df$temperature.18 <- clean.df$temperature-18
  #clean.df$Signal <- sign(clean.df$temperature.18)
  #df <- as.data.table(clean.df)
  #df[,cum.temp := cumsum(temperature.18)*Signal,.(rleid(Signal))] #pas besoin de l'enregistrer dans une autre variable df
  #clean.df <- as.data.frame(df)
  #clean.df <- clean.df[,!colnames(clean.df)%in% c("Signal","temperature.18")]
}
# cumsum(clean.df$temperature.18)
# temp.test <- rep(0, nrow(clean.df))
# temp.test <- (clean.df$temperature.18>0)*1
# temp.test[which(temp.test==0)] <- -1

# lapply(split(clean.df$temperature.18, clean.df$temperature.18 < 0), sum)
# 
# replace(clean.df$temperature.18, which((clean.df$temperature.18>0)), NA) %>% cumsum %>% {.==NA}
# cumsum(clean.df$temperature.18*temp.test)
# lm(Load_Mw~ bs(temperature.18,3), clean.df[which(clean.df$temperature.18<0),])  %>% plot()
# plot(clean.df$temperature.18, clean.df$Load_Mw)
# plot(clean.df$temperature.18[which(clean.df$temperature.18<0)], clean.df$Load_Mw[which(clean.df$temperature.18<0)])

# V2 de chauffage
{
  clean.df$under_18_logical <- rep(F,nrow(clean.df))
  clean.df$under_18_logical[which(clean.df$temperature < 18)] <- T
  
  clean.df$cumsum_under_18 <- cumsum_reset(clean.df$under_18_logical,as.numeric(clean.df$under_18_logical))
  clean.df$cumsum_over_18 <- cumsum_reset(clean.df$under_18_logical==F,as.numeric(clean.df$under_18_logical==F))
  
  clean.df$Chauffage <- rep(0,nrow(clean.df))
  clean.df$Climatisation <- rep(0,nrow(clean.df))
  clean.df$Chauffage[which(clean.df$cumsum_under_18-3 > 0 )] <- 1
  clean.df$Climatisation[which(clean.df$cumsum_over_18-3 > 0 )] <- 1 
  
  clean.df$integer_chauffage <- (18 - clean.df$temperature)*clean.df$Chauffage
  clean.df$integer_climatisation <- (clean.df$temperature - 18)*clean.df$Climatisation
  
  clean.df <- clean.df[,-which(colnames(clean.df) %in% c('under_18_logical','cumsum_under_18','cumsum_over_18','Chauffage','Climatisation'))]
}

# Difference avec temperature moyenne du mois ----
{
  temp_month_mean.df <- with(clean.df,aggregate(temperature,by=list(Month),mean))
  colnames(temp_month_mean.df) <- c('Month','Mean_temp_Month')
  clean.df <- left_join(clean.df,temp_month_mean.df,by='Month',suffix=c('','.y'))
  clean.df$diff_mean_temp_month <- clean.df$temperature - clean.df$Mean_temp_Month
  clean.df <- clean.df[,-which(colnames(clean.df) %in% 'Mean_temp_Month')] 
}


# holiday ----
#clean.df$holiday <- isHoliday(x=timeDate(clean.df$Date.s),holidays='TSX')*1

# liste de tous les conges feries selon le calendrier TSX. Il manque seulement family day & le boxing day est pas exact (quand ca tombe un samedi ou un dimanche c'est reporte)
{
  holidays <- c(as.POSIXct(holiday(2003:2016,'NewYearsDay'),tz='UTC'),
                as.POSIXct(c('2003-02-17','2004-02-16','2005-02-21','2006-02-20','2007-02-19','2008-02-18','2009-02-19','2010-02-15','2011-02-21','2012-02-20','2013-02-18','2014-02-17','2015-02-16','2016-02-15'),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "GoodFriday"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "CAVictoriaDay"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "CACanadaDay"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "CACivicProvincialHoliday"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "CALabourDay"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "CAThanksgivingDay"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "ChristmasDay"),tz='UTC'),
                as.POSIXct(holiday(2003:2016, "BoxingDay"),tz='UTC'))
  
  clean.df$holiday <- rep(0,nrow(clean.df))
  clean.df$holiday[which(as.POSIXct(date(clean.df$Date.s)) %in% holidays)] <- 1
}


# heure souper ----
#clean.df$souper <- (clean.df$Hour>=17 & clean.df$Hour<=21)*1

# jours different ----

#clean.df$Day.of.the.week <- (clean.df$weekday>=17 & clean.df$Hour<=21)*1

# Load_Pj de l'annee d'avant
prev_year <- ad.df[which(ad.df$Secteur == 'Residentiel'),c('Year','Load_PJ')]
colnames(prev_year) <- c('Year','Prev_Year_Load_Pj')
clean.df <- left_join(clean.df,prev_year,by='Year')

ad.df
clean.df <- clean.df[,-which(colnames(clean.df)=='Date.s')]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyse des variables explicatives ----

colnames(clean.df)

# Hour
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[1])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[1])
# *****

# Year
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[3])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[3])
# ****

# Month
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[4])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[4])
# ****

# precipitation
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[5])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[5])
# Peu de lien

# temperature
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[6])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[6])
# *****

# irradiance_surface
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[7])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[7])
# Aucun lien

# irradiance_sommet_atmosphere
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[8])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[8])
# Aucun lien

# chute.de.neige
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[9])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[9])
# Peu de lien

# profondeur_neige
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[10])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[10])
#Peu de lien

# couverture_nuage
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[11])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[11])
# Aucun lien

# densite_air
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[12])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[12])
# *****

# Day
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[13])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[13])
# Aucun lien

# weekday
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[14])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[14])
# Petit lien

# mean_by_wday_MW
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[15])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[15])
# Grand lien!!

# year_day
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[16])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[16])
# Grand lien

# mean_by_yearday_Mw
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[17])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[17])
# Peu de lien

# Weekend
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[18])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[18])
# Petit lien

# integer_chauffage
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[19])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[19])
# lien!

# integer_climatisation
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[20])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[20])
# lien!

# diff_mean_temp_month
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[21])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[21])
# Aucun lien

# holiday
plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[22])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[22])
# Lien

plot(with(clean.df,aggregate(Load_Mw,by=list(Year),mean)),type='l')

plot(x=clean.df[,which(colnames(clean.df) == colnames(clean.df)[23])],y=clean.df$Load_Mw,xlab=colnames(clean.df)[23])

#clean.df <- clean.df[,-which(colnames(clean.df) %in% c('precipitation',
                                                       #'irradiance_surface',
                                                      # 'irradiance_sommet_atmosphere',
                                                       #'chute.de.neige',
                                                      # 'profondeur_neige',
                                                      # 'couverture_nuage',
                                                      # 'densite_air',
                                                      # 'Day',
                                                       #'diff_mean_temp_month'))]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREANALYSE: ----
# Validations 
nrow(hd.df) == nrow(w.df)
nrow(hd.df)
nrow(ad.df)

# Donnees manquantes
sapply(hd.df,function(X) sum(is.na(X))) # Aucune donnee manquante
sapply(w.df,function(X) sum(is.na(X))) # Aucune donnee manquante
sapply(ad.df,function(X) sum(is.na(X))) # Aucune donne manquante


# plot(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."],type='l') # ne fonctionne pas, aucune colonne de nom Total.Energy.Use.from.Electricity..MW.
# lines(hd.df[hd.df$Hour ==2,"Total.Energy.Use.from.Electricity..MW."],col='red')
#hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 


# hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',] # ne fonctionne pas 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# M O D E L E S




# plot(x=hour.df$temperature,y=hour.df$Load_Mw)
# 
 {
#   # Modele 1 : Base sur cet article (https://freakonometrics.hypotheses.org/52081) 
#   
#   plot(hd.df[hd.df$Year == 2003,3],type='l')
#   model1 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year, data=hd.df, subset = which(hd.df$Year %in% c(2003:2005))) # il faut mettre plus d'une valeur de Year, sinon le lm exclus la variable explicative 
#   summary(model1) #Year est NA dans le modele,  normal?
#   
#   new_data <- hd.df[hd.df$Year == 2016,]
#   p = predict(model1,newdata=new_data[1:100,]) #
#   plot(new_data[1:100,3],type='l')
#   lines(p[1:100],col='red')
#   # Clairement pas great comme modele, on va ajouter la temperature
#   
#   
#   # Modele 2 : Ajout de la temperature 
#   class(hd.df$Date.s)
#   class(w.df$Date)
#   # w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm() # pas necessaire, ligne en haut qui fait la meme chose
#   
#   
#   
#   
#   plot(x=hour.df$temperature,y=hour.df$Load_Mw)
#   
#   model2 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year + bs(temperature),data=hour.df[hour.df$Year != 2016,]) 
#   # j'ai mis bs pour la temperature pcq poly ne marchait pas
#   summary(model2)
#   
#   new_data <- hour.df[hour.df$Year == 2016,]
#   p = predict(model2,newdata=new_data[1:110,c(2,5,4,8)])
#   plot(new_data[1:100,3],type='l')
#   lines(p[1:100],col='red')
#   # Deja beaucoup mieux
#   
#   # Modele 3 : premier essaie pour l'arbre 
#   hour.df.test <- hour.df[-train,]
#   
#   model3 <- tree(Load_Mw ~ .,data=hour.df,subset=train)
#   summary(model3)
#   plot(model3)
#   text(model3,pretty=0)
#   
#   pred <- predict(model3,hour.df.test)
#   plot(hour.df.test[1:100,3],type='l')
#   lines(pred[1:100],col='red')
#   
#   # On va essayer de prune l'arbre pour avoir des meilleurs resultats
#   cv.model3 <- cv.tree(model3)
#   # dev correspond au cross-validation error rate
#   # On utilise la cross validation pour savoir si du pruning est necessaire. Puisque le cross validation utilise un subtree, un devrait prune je pense
#   plot(cv.model3$size,cv.model3$dev,type='b') # Aucune idee ce que signifie ce graph, c'etait ce qui faisait dans le livre
#   plot(cv.model3$k,cv.model3$dev,type='b')  # Aucune idee ce que signifie ce graph, c'etait ce qui faisait dans le livre
#   
#   prune.model3 <- prune.tree(model3,best=5)
#   plot(prune.model3)
#   text(prune.model3,pretty=0)
#   
#   pred <- predict(prune.model3,hour.df.test)
#   plot(pred,hour.df.test[,'Load_Mw'])
#   abline(0,1)
#   # Clairement pas obtimal
#   MSE <- mean((pred-hour.df.test[,'Load_Mw'])^2) # Immense MSE
#   sqrt(MSE) # Les predictions sont around 1978 Mw de la vraie valeur
#   
#   
#   # Modele 4 : Premier essaie de random forest 
#   
#   #model4 <- randomForest(Load_Mw~.,data=na.exclude(hour.df),subset=train,mtry=13,importance=T)
#   #randomForest(Load_Mw~.,data=na.exclude(hour.df),subset=train,importance=T)
#   
#   mini.df <- hour.df[train,c('Hour','Year','Load_Mw','temperature','profondeur_neige','densite_air')]
#   
#   model4 <- randomForest(Load_Mw ~ .,data=na.omit(mini.df), mtry = length(colnames(mini.df))/3, importance = TRUE, ntree = 50)
#   summary(model4)
#   importance(model4)
#   
#   new_data <- hour.df[-train,c('Hour','Year','Load_Mw','temperature','profondeur_neige','densite_air')]
#   pred.rf <- predict(model4,newdata=new_data)
#   MSE.rf <- mean((pred.rf-mini.df$Load_Mw)^2) # Encore immense...
#   sqrt(MSE)
#   varImpPlot(model4)
#   
#   plot(new_data[300:400,'Load_Mw'],type='l')
#   lines(pred.rf[300:400],col='red')
#   # Not bad!!
#   
#   # Modele 5 : Essayons de rouler une random forest en parallel pour voir ce que ca donne quand on la laisse decider des variables
#   cores <- 6
#   cl <- makeCluster(cores)
#   registerDoParallel(cores)
#   getDoParWorkers() # Just checking, how many workers you have
#   
#   model5 <- randomForest(Load_Mw~.,data=hour.df,subset=train,importance=T)
#   importance(model5) # On remarque beaucoup de redondance, genre surement pas besoin de Date.s, Date, Hour, Year, Month, Group.1... je vais faire une base cleaner lundi
#   # Je pensais que la random forest utiliserait juste les variables pertinentes comme le tree l'avais fait, mais on dirait pas finalement...
#   pred.rf.2 <- predict(model5,newdata=hour.df[-train,])
#   MSE.rf.2 <- mean((pred.rf.2-hour.df[-train,'Load_Mw'])^2)
#   sqrt(MSE.rf.2) # Considerablement plus petit que ce qu'on a eu jusqu'a present!
#   
#   new_data <- hour.df[-train,]
#   plot(new_data[1:100,'Load_Mw'],type='l')
#   lines(pred.rf.2[1:100],col='red')
#   
#   stopCluster(cl)
#   
 } # ABANDONS

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modele 6 : Random Forest mais avec une base de donnee clean ----

{
  {
    if(detectCores()==8){
      cores <- 7
      num.of.tree <- 50
    } else {
      cores <- 12
      num.of.tree <- 500
        }
    cl <- makeCluster(cores)
    registerDoParallel(cores)
    getDoParWorkers() # Just checking, how many workers you have 
  }
  
  model6 <- foreach(ntree=rep(floor(num.of.tree/cores), cores), .combine=randomForest::combine,
                    .multicombine=TRUE, .packages='randomForest') %dopar% {
                      randomForest(Load_Mw~.,data= na.omit(clean.df),
                                   subset=train,
                                   importance=T,
                                   ntree=ntree)
                    }
  
  stopCluster(cl)
}
importance(model6)


{
  pred.rf <- predict(model6,newdata=clean.df[-train,-which(colnames(clean.df) %in% 'Load_Mw')])
  res <- pred.rf - clean.df[-train,'Load_Mw']
  MSE.rf <- mean(res^2)
  sqrt(MSE.rf) 
}

new_data <- clean.df[-train,]


pige <- sample.int(nrow(new_data)-7,1)
data.plot <- (pige-7):pige

which(new_data$Month == 10 & new_data$Year == 2013) %>% 
  {plot(new_data[.,'Load_Mw'],type='l')
  lines(pred.rf[.],col='red')}


plot(new_data[which(new_data$Month == 10 & new_data$Year == 2013),'Load_Mw'],type='l')
lines(pred.rf[which(new_data$Month == 10 & new_data$Year == 2013)],col='red')



mauvais_res.df <- new_data[which(abs(res) > quantile(abs(res))[4]),]
table(mauvais_res.df$Hour)
table(mauvais_res.df$Year)
table(mauvais_res.df$Month)
table(mauvais_res.df$Hour,mauvais_res.df$Month)
table(mauvais_res.df$Day)
table(mauvais_res.df$weekday)
table(mauvais_res.df$holiday)



test <- with(clean.df,aggregate(Load_Mw,by=list(weekday,Month),mean))
test2 <- with(clean.df,aggregate(Load_Mw,by=list(weekday,Month),quantile))
plot(test$x,type='l')
nrow(hour.df)

# acf des residus :
acf(res,lag=24)
acf(res,lag=24*7) # Nos residus ont l'air corrélés pas mal...

plot(clean.df[-train,'Load_Mw'],res)

#corAR1(acf(res,lag=1,plot=F)$acf[2],form=~.)


# Pour voir les variables pertinentes
reg <- glm(Load_Mw~.,subset = train,clean.df,family='gaussian')
summary(reg)

plot(predict(reg),col='red')
lines(clean.df$Load_Mw)

reg2 <- step(reg,scale=0,trace=F)
summary(reg2)

pred.rf <- predict(reg2,newdata=clean.df[-train,-which(colnames(clean.df) %in% 'Load_Mw')])
res <- pred.rf - clean.df[-train,'Load_Mw']
MSE.rf <- mean(res^2)
sqrt(MSE.rf) 


{
  # Modele 7 : random forest avec database en format ts 
  
  #clean.test.ts <- ts(clean.df,start=c(2003,1),end=c(2016,12),freq=24*365.5)
  
  
  #{
   # cores <- 6
    #cl <- makeCluster(cores)
    #registerDoParallel(cores)
    #getDoParWorkers() 
  #}
  
  #model7 <- randomForest(Load_Mw~.,data=clean.test.ts,subset=train,importance=T,ntree=50)
  
  #stopCluster(cl) 
  
  
  #importance(model7)
  
  #{
   # pred.rf.3 <- predict(model7,newdata=clean.test.ts[-train,])
    #MSE.rf.3 <- mean((pred.rf.3-clean.test.ts[-train,'Load_Mw'])^2)
    #sqrt(MSE.rf.3) # Plus bas qu'avec le data.frame, ce modele est donc meilleur!
  #}
  
  
  #new_data <- clean.test.ts[-train,]
  #plot(new_data[100:200,'Load_Mw'],type='l')
  #lines(pred.rf.3[100:200],col='red')
  
  # Modele 8 : Random forest en format ts test 2 --
  
  #clean.test.ts
  
  #clean.2.ts <- clean.test.ts[,-which(colnames(clean.test.ts) %in% c('Hour','Year','Month','Day'))]
  
  #{
  #  cores <- 6
  #  cl <- makeCluster(cores)
  #  registerDoParallel(cores)
  #  getDoParWorkers() 
  #}
  
  #model8 <- randomForest(Load_Mw~.,data=clean.2.ts,subset=train,importance=T,ntree=50)
  
  #stopCluster(cl) 
  
  #importance(model8)
  
  #{
   # pred.rf.3 <- predict(model8,newdata=clean.2.ts[-train,])
  #  MSE.rf.3 <- mean((pred.rf.3-clean.2.ts[-train,'Load_Mw'])^2)
   # sqrt(MSE.rf.3) # Plus bas qu'avec le data.frame, ce modele est donc meilleur!
  #}
  
  
  #new_data <- clean.2.ts[-train,]
  #plot(new_data[1:100,'Load_Mw'],type='l')
  #lines(pred.rf.3[1:100],col='red')
  # Pas tres bon finalement, on va rester avec le model7
  
  
  
  
  # Modele 6.gm, bestglm ———— NE FONTIONNE PAS!! --- 
  #{hour.y.df <- hour.df
  #  hour.y.df$y <- hour.y.df$Load_Mw
  #  hour.y.df <- hour.y.df[,!colnames(hour.y.df) %in% c("Load_Mw", "Hour", "Year")]
  #}
  #hour.y.df %>% str()
  #best.hour.y.df <- bestglm(Xy = hour.y.df, family = exponential, IC = "AIC", method = "exhaustive")  
} # ABANDONS



