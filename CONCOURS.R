# options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library ----
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod",
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate", "purrr", "data.table", "bestglm",
                "xts", "forecast", "tseries", # for time series object
                "corrgram",#correlation gram, semblable a acf
                "opera", #package arthur
                "keras", # info sur son utilisation: https://www.datacamp.com/community/tutorials/keras-r-deep-learning
                "tensorflow",
                'tree', # pour faire des arbres
                'randomForest', 
                'doParallel', "foreach",
                'timeDate',
                'bestglm',
                'chron',
                'rjson'
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

#-----
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
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    # cbind(hour.ls[[my.year]], 
    #       ad.p.df[ad.p.df$Secteur == "Residentiel",] %>% 
    #         {.[.$Year == as.numeric(my.year),!colnames(.) %in% c("Year", "Secteur")]} %>%  #/ nrow(hour.ls[[my.year]])}
    #         {.[,"proportion.conso"]}
    #       )
  }) %>% reduce(bind_rows)
}
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Optimisation 
# hour.df$day.week <- wday(hour.df$Date.s)
# hour.df$day.week
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Visualisation d'une serie
acf(hour.df$Load_Mw, lag=24) #autocorrelation par 24h
acf(hour.df$Load_Mw, lag=24*7*3)
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
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# M O D E L E S

# On va utiliser 70% des donnees pour le training :
train <- 1:(ceiling(0.7*nrow(hour.df)))


plot(x=hour.df$temperature,y=hour.df$Load_Mw)

{
  # Modele 1 : Base sur cet article (https://freakonometrics.hypotheses.org/52081) 
  
  plot(hd.df[hd.df$Year == 2003,3],type='l')
  model1 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year, data=hd.df, subset = which(hd.df$Year %in% c(2003:2005))) # il faut mettre plus d'une valeur de Year, sinon le lm exclus la variable explicative 
  summary(model1) #Year est NA dans le modele,  normal?
  
  new_data <- hd.df[hd.df$Year == 2016,]
  p = predict(model1,newdata=new_data[1:100,]) #
  plot(new_data[1:100,3],type='l')
  lines(p[1:100],col='red')
  # Clairement pas great comme modele, on va ajouter la temperature
  
  
  # Modele 2 : Ajout de la temperature 
  class(hd.df$Date.s)
  class(w.df$Date)
  # w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm() # pas necessaire, ligne en haut qui fait la meme chose
  
  
  
  
  plot(x=hour.df$temperature,y=hour.df$Load_Mw)
  
  model2 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year + bs(temperature),data=hour.df[hour.df$Year != 2016,]) 
  # j'ai mis bs pour la temperature pcq poly ne marchait pas
  summary(model2)
  
  new_data <- hour.df[hour.df$Year == 2016,]
  p = predict(model2,newdata=new_data[1:110,c(2,5,4,8)])
  plot(new_data[1:100,3],type='l')
  lines(p[1:100],col='red')
  # Deja beaucoup mieux
  
  # Modele 3 : premier essaie pour l'arbre 
  hour.df.test <- hour.df[-train,]
  
  model3 <- tree(Load_Mw ~ .,data=hour.df,subset=train)
  summary(model3)
  plot(model3)
  text(model3,pretty=0)
  
  pred <- predict(model3,hour.df.test)
  plot(hour.df.test[1:100,3],type='l')
  lines(pred[1:100],col='red')
  
  # On va essayer de prune l'arbre pour avoir des meilleurs resultats
  cv.model3 <- cv.tree(model3)
  # dev correspond au cross-validation error rate
  # On utilise la cross validation pour savoir si du pruning est necessaire. Puisque le cross validation utilise un subtree, un devrait prune je pense
  plot(cv.model3$size,cv.model3$dev,type='b') # Aucune idee ce que signifie ce graph, c'etait ce qui faisait dans le livre
  plot(cv.model3$k,cv.model3$dev,type='b')  # Aucune idee ce que signifie ce graph, c'etait ce qui faisait dans le livre
  
  prune.model3 <- prune.tree(model3,best=5)
  plot(prune.model3)
  text(prune.model3,pretty=0)
  
  pred <- predict(prune.model3,hour.df.test)
  plot(pred,hour.df.test[,'Load_Mw'])
  abline(0,1)
  # Clairement pas obtimal
  MSE <- mean((pred-hour.df.test[,'Load_Mw'])^2) # Immense MSE
  sqrt(MSE) # Les predictions sont around 1978 Mw de la vraie valeur
  
  
  # Modele 4 : Premier essaie de random forest 
  
  #model4 <- randomForest(Load_Mw~.,data=na.exclude(hour.df),subset=train,mtry=13,importance=T)
  #randomForest(Load_Mw~.,data=na.exclude(hour.df),subset=train,importance=T)
  
  mini.df <- hour.df[train,c('Hour','Year','Load_Mw','temperature','profondeur_neige','densite_air')]
  
  model4 <- randomForest(Load_Mw ~ .,data=na.omit(mini.df), mtry = length(colnames(mini.df))/3, importance = TRUE, ntree = 50)
  summary(model4)
  importance(model4)
  
  new_data <- hour.df[-train,c('Hour','Year','Load_Mw','temperature','profondeur_neige','densite_air')]
  pred.rf <- predict(model4,newdata=new_data)
  MSE.rf <- mean((pred.rf-mini.df$Load_Mw)^2) # Encore immense...
  sqrt(MSE)
  varImpPlot(model4)
  
  plot(new_data[300:400,'Load_Mw'],type='l')
  lines(pred.rf[300:400],col='red')
  # Not bad!!
  
  # Modele 5 : Essayons de rouler une random forest en parallel pour voir ce que ca donne quand on la laisse decider des variables
  cores <- 6
  cl <- makeCluster(cores)
  registerDoParallel(cores)
  getDoParWorkers() # Just checking, how many workers you have
  
  model5 <- randomForest(Load_Mw~.,data=hour.df,subset=train,importance=T)
  importance(model5) # On remarque beaucoup de redondance, genre surement pas besoin de Date.s, Date, Hour, Year, Month, Group.1... je vais faire une base cleaner lundi
  # Je pensais que la random forest utiliserait juste les variables pertinentes comme le tree l'avais fait, mais on dirait pas finalement...
  pred.rf.2 <- predict(model5,newdata=hour.df[-train,])
  MSE.rf.2 <- mean((pred.rf.2-hour.df[-train,'Load_Mw'])^2)
  sqrt(MSE.rf.2) # Considerablement plus petit que ce qu'on a eu jusqu'a present!
  
  new_data <- hour.df[-train,]
  plot(new_data[1:100,'Load_Mw'],type='l')
  lines(pred.rf.2[1:100],col='red')
  
  stopCluster(cl)
  
} # ABANDONS


# Modele 6 : Random Forest mais avec une base de donnee clean ----
hour.df
clean.df <- hour.df
clean.df$Day <- day(clean.df$Date.s)
clean.df$weekday <- wday(clean.df$Date.s)

# for(i in 1:nrow(clean.df)){
#   clean.df[i,'Weekend'] <- if(isWeekend(clean.df[i,'Date.s'])[[1]]){1}else{0}
#   clean.df[i,'snow'] <- if(clean.df[i,'profondeur_neige'] > 0){1}else{0}
# }

{
  clean.df$Weekend <- clean.df$snow <- clean.df$dummy_temp <- rep(0, nrow(clean.df))
  clean.df$snow[which(clean.df$profondeur_neige > 0)] <- 1
  clean.df$Weekend[which(isWeekend(clean.df$Date.s))] <- 1
  clean.df$dummy_temp[which(clean.df$temperature > 20)] <- 1
}

# Tests de holiday qui n'ont pas marché
#isHoliday('Canada',as.Date(clean.df[100,'Date.s']))[[1]]
#isHoliday(x=as.Date(clean.df$Date.s),holidays='Canada/TSX')

clean.df <- clean.df[,-which(colnames(clean.df)=='Date.s')]

{
  {
    cores <- if(detectCores()==8){7} else {11}
    cl <- makeCluster(cores)
    registerDoParallel(cores)
    getDoParWorkers() # Just checking, how many workers you have 
  }
  
  model6 <- foreach(ntree=rep(floor(50/cores), cores), .combine=randomForest::combine,
                    .multicombine=TRUE, .packages='randomForest') %dopar% {
                      randomForest(Load_Mw~.,data=clean.df,
                                   subset=train,
                                   importance=T,
                                   ntree=ntree)
                    }
  
  stopCluster(cl)
}
importance(model6)


{
  pred.rf.3 <- predict(model6,newdata=clean.df[-train,])
  MSE.rf.3 <- mean((pred.rf.3-clean.df[-train,'Load_Mw'])^2)
  sqrt(MSE.rf.3) 
}

new_data <- clean.df[-train,]
plot(new_data[1:100,'Load_Mw'],type='l')
lines(pred.rf.3[1:100],col='red')

new_data[1:100,]

{
  # Modele 7 : random forest avec database en format ts 
  
  clean.test.ts <- ts(clean.df,start=c(2003,1),end=c(2016,12),freq=24*365.5)
  
  
  {
    cores <- 6
    cl <- makeCluster(cores)
    registerDoParallel(cores)
    getDoParWorkers() 
  }
  
  model7 <- randomForest(Load_Mw~.,data=clean.test.ts,subset=train,importance=T,ntree=50)
  
  stopCluster(cl) 
  
  
  importance(model7)
  
  {
    pred.rf.3 <- predict(model7,newdata=clean.test.ts[-train,])
    MSE.rf.3 <- mean((pred.rf.3-clean.test.ts[-train,'Load_Mw'])^2)
    sqrt(MSE.rf.3) # Plus bas qu'avec le data.frame, ce modele est donc meilleur!
  }
  
  
  new_data <- clean.test.ts[-train,]
  plot(new_data[100:200,'Load_Mw'],type='l')
  lines(pred.rf.3[100:200],col='red')
  
  # Modele 8 : Random forest en format ts test 2 --
  
  clean.test.ts
  
  clean.2.ts <- clean.test.ts[,-which(colnames(clean.test.ts) %in% c('Hour','Year','Month','Day'))]
  
  {
    cores <- 6
    cl <- makeCluster(cores)
    registerDoParallel(cores)
    getDoParWorkers() 
  }
  
  model8 <- randomForest(Load_Mw~.,data=clean.2.ts,subset=train,importance=T,ntree=50)
  
  stopCluster(cl) 
  
  importance(model8)
  
  {
    pred.rf.3 <- predict(model8,newdata=clean.2.ts[-train,])
    MSE.rf.3 <- mean((pred.rf.3-clean.2.ts[-train,'Load_Mw'])^2)
    sqrt(MSE.rf.3) # Plus bas qu'avec le data.frame, ce modele est donc meilleur!
  }
  
  
  new_data <- clean.2.ts[-train,]
  plot(new_data[1:100,'Load_Mw'],type='l')
  lines(pred.rf.3[1:100],col='red')
  # Pas tres bon finalement, on va rester avec le model7
  
  
  
  
  # Modele 6.gm, bestglm ———— NE FONTIONNE PAS!! --- 
  {hour.y.df <- hour.df
    hour.y.df$y <- hour.y.df$Load_Mw
    hour.y.df <- hour.y.df[,!colnames(hour.y.df) %in% c("Load_Mw", "Hour", "Year")]
  }
  hour.y.df %>% str()
  best.hour.y.df <- bestglm(Xy = hour.y.df, family = exponential, IC = "AIC", method = "exhaustive")  
} # ABANDONS



