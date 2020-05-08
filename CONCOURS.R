# options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod",
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate", "purrr", "data.table",
                "opera", #package arthur
                "keras", # info sur son utilisation: https://www.datacamp.com/community/tutorials/keras-r-deep-learning
                'tree', # pour faire des arbres
                'randomForest'
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
w.df[c(which(duplicated(w.df$Date))-1,which(duplicated(w.df$Date))),]


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
# Arrangement des doublons PAS FINI >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm()
w.df <- w.df[,!colnames(w.df) %in% "Date"] # pour ne pas creer de confusion entre les bases de donnees
w.df <- aggregate(w.df, by = list(w.df$Date.s), mean)

identical(length(w.df$Date.s), length(unique(w.df$Date.s))) # All work!!
# n_occur <- data.frame(table(w.df$Date.s)) %>%
#   {.[.$Freq > 1,]}
# n_occur$Var1 <- n_occur$Var1 %>% as.character %>% ymd_hms()
# n_occur$Freq <- n_occur$Freq %>% cumsum
# to_change <- which((w.df$Date.s %in% n_occur$Var1) == T)
# to_change[1:last(to_change)] %>% list()
# seq(first(to_change), last(to_change))
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MERGE des bases de donnees weather et demande aux heures

#  on remarque que les bases de donnees ont des differences:
list(w.df.dif = which(!(hd.df$Date.s %in% w.df$Date.s)) %>% hd.df$Date.s[.],
     hd.df.dif = which(!(w.df$Date.s %in% hd.df$Date.s)) %>% w.df$Date.s[.])
# Ainsi, on fait l'union des deux bases de donnees avec all=T —— idk si left_join ne le faisait pas, pour verifier : identical(merge(hd.df, w.df, by = "Date.s", all=T), left_join(hd.df,w.df,by = 'Date.s'))
hour.df <- merge(hd.df, w.df, by = "Date.s", all.x=T)  # On va merge les 2 df par heure pour faciliter les modeles
which(is.na(hour.df)) %>% hour.df[.,]
# On retire les donnees ou Load_Mw est NA
hour.df <- hour.df[!is.na(hour.df$Load_Mw),]

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
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Visualisation d'une serie
acf(hour.df$Load_Mw) 

# Mesure s'il y a une constance dans la serie (aka, une non croissance p/r au temps)
# plot(lm(Load_Mw~Date.s, data = hour.df))# %>% summary()
{plot(hour.df$Date.s, hour.df$Load_Mw,pch='.')
lm.fit <- lm(Load_Mw~Date.s, data = hour.df)
p.lm <- lm.fit %>% predict(se.fit = T) 
seq(min(hour.df$Date.s), max(hour.df$Date.s), by = "hour") %>% lines(., p.lm$fit, col="red")
# title()
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
hd.df[hd.df$Total.Energy.Use.from.Electricity..MW. == min(hd.df[hd.df$Hour==1,"Total.Energy.Use.from.Electricity..MW."]) & hd.df$Hour==1, ] 


# hd.df[hd.df$Date == '15-août-03' | hd.df$Date == '14-août-03',] # ne fonctionne pas 



# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# M O D E L E S

# On va utiliser 70% des donnees pour le training :
train_max <- 0.7*nrow(hour.df)


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
w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm()




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
train <- 1:train_max
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
model4 <- randomForest(Load_Mw~.,data=na.exclude(hour.df),subset=train,mtry=13,importance=T)



