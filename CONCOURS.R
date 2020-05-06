options(java.parameters = "-Xmx8000m") #afin de donner plus de heap space a java
library(tidyverse)
library(readxl)
library(xlsx)

# test <- read.csv(paste0(getwd(),"/Database/hourly_demand_MANUELLEMENT.csv"), encoding="UTF-8")
test2 <- read.csv("/home/gabriel/Github/prevoir-demande-horaire-en-electricite-en-ontario/Database/hourly_demand.csv", encoding = "UTF-8", sep = ";")
test.wth <- read.csv("/home/gabriel/Github/prevoir-demande-horaire-en-electricite-en-ontario/Database/hourly_weather.csv", encoding = "UTF-8", sep = ";", header = T)
test %>% head()
test2 %>% head()
