# options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
# Library
{
  list.of.packages <- c("MASS", "lmtest", "nortest", "car", "splines", "AER", "COUNT", "pROC", "plotROC", "verification", "ROCR", "aod", "vcd", "statmod",
                "tidyverse", "stringr", "reshape2", "ggplot2", "plotly", "corrplot", "lubridate", "purrr", "data.table",
                "opera", #package arthur
                "keras" #package KNN
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
# MERGE des bases de donnees weather et demande aux heures
w.df$Date.s <- w.df$Date %>% as.character() %>% ymd_hm()
w.df <- w.df[,!colnames(w.df) %in% "Date"] # pour ne pas creer de confusion entre les bases de donnees
#  on remarque que les bases de donnees ont des differences:
list(w.df.dif = which(!(hd.df$Date.s %in% w.df$Date.s)) %>% hd.df$Date.s[.],
     hd.df.dif = which(!(w.df$Date.s %in% hd.df$Date.s)) %>% w.df$Date.s[.])
# Ainsi, on fait l'union des deux bases de donnees avec all=T —— idk si left_join ne le faisait pas, pour verifier : identical(merge(hd.df, w.df, by = "Date.s", all=T), left_join(hd.df,w.df,by = 'Date.s'))
hour.df <- merge(hd.df, w.df, by = "Date.s", all.x=T)  # On va merge les 2 df par heure pour faciliter les modeles
# On retire les donnees ou Load_Mw est NA
hour.df <- hour.df[!is.na(hour.df$Load_Mw),]

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Proportion de la consommation d'electricite par le Residentiel PAS FINI >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
{
  ad.ls <- split(ad.df, ad.df$Year)
  sum.year.conso <- purrr::map(ad.ls, ~.x$Load_PJ %>% sum)
  prop.conso.df <- 
    purrr::map2(ad.ls, sum.year.conso,
                ~sapply(1:nrow(.x),function(i){
                  .x$Load_PJ[i]/.y
                  })
                )
  
  
  #%>% reduce(c) %>%
    tibble(proportion.consommation.e= .,
           # Year = names(ad.ls),
           Secteur = "Residentiel"
           )
  prop.conso.df
  mutate_all(ad.df, prop.conso.df, by="Secteur",all.x=T)
  # ad.df$proportion.consommation.e <- if(ad.df$Secteur =="Residentiel")
}; prop.conso
lapply(seq_along(ad.ls),function(i){
  apply(ad.ls[[i]], function(ad.ls.irow){
    var.tmp <- rep(0;1:unique(ad.ls.irow$Secteur)
    if(ad.ls.irow$Secteur %in% "Residentiel"){
      cbind(.xi, proportion.consommation = .y)
    } else {cbind(.x, proportion.consommation = 0)}
  })
})


map2(ad.ls, prop.conso,
     ~sapply(.x, function(.xi){
       if(.xi$Secteur %in% "Residentiel"){
         cbind(.xi, proportion.consommation = .y)
       } else {cbind(.x, proportion.consommation = 0)}
     })
)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



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



# TEST : Base sur cet article (https://freakonometrics.hypotheses.org/52081)
plot(hd.df[hd.df$Year == 2003,3],type='l')
model1 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year, data=hd.df, subset = which(hd.df$Year %in% c(2003:2005))) # il faut mettre plus d'une valeur de Year, sinon le lm exclus la variable explicative 
summary(model1) #Year est NA dans le modele,  normal?

new_data <- hd.df[hd.df$Year == 2016,]
p = predict(model1,newdata=new_data[1:100,]) #
plot(new_data[1:100,3],type='l')
lines(p[1:100],col='red')
# Clairement pas great comme modele, on va ajouter la temperature




plot(x=hour.df$temperature,y=hour.df$Load_Mw)

model.2 <- lm(Load_Mw ~ poly(Hour,3) + poly(Month,3) + Year + bs(temperature),data=hour.df[hour.df$Year == 2003,]) 
# j'ai mis bs pour la temperature pcq poly ne marchait pas
summary(model.2)

new_data <- hour.df[hour.df$Year == 2004,]
p = predict(model.2,newdata=new_data[1:110,c(2,5,4,8)])
plot(new_data[1:100,3],type='l')
lines(p[1:100],col='red')
# Deja beaucoup mieux

