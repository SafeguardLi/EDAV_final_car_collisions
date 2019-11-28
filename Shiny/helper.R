# "C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/data/victim.csv"

## set up
library(tidyverse)
library(mapdeck)
library(sf)
Sys.setenv("plotly_username"="SafeguardLi")
Sys.setenv("plotly_api_key"="JDJpqet5WWuBC3kDQaQc")
key <- "pk.eyJ1IjoiZ2VvcmdlbGVlMjAxOSIsImEiOiJjazMyZTV1Z3UwajMwM21wbXFpbjdjN2E1In0.FxeN5dw--xCWmVSwMDH4BA"

## read data
data <- read.csv("C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/data/Motor_Vehicle_Collisions_-_Crashes.csv", header=T, na.strings=c("","NA"))
crash <- data%>%select(COLLISION_ID, ACCIDENT.DATE, ACCIDENT.TIME, lat = LATITUDE, lon = LONGITUDE, LOCATION, NUMBER.OF.PERSONS.INJURED, NUMBER.OF.PERSONS.KILLED, CONTRIBUTING.FACTOR.VEHICLE.1, VEHICLE.TYPE.CODE.1)
crash$ACCIDENT.DATE <- as.Date(crash$ACCIDENT.DATE, "%m/%d/%Y")
crash$CONTRIBUTING.FACTOR.VEHICLE.1 <- tolower(crash$CONTRIBUTING.FACTOR.VEHICLE.1)
crash$VEHICLE.TYPE.CODE.1 <- tolower(crash$VEHICLE.TYPE.CODE.1)
crash <- crash%>%mutate(victims = NUMBER.OF.PERSONS.INJURED+NUMBER.OF.PERSONS.KILLED)
crash <- crash[order(crash$ACCIDENT.DATE),]

## subset data: total victims
if(!file.exists("C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/data/total_victim.csv")){
  
  ## write subset of total victims data
  t1 = Sys.time()
  victim <- crash
  victim <- victim[which(victim$victims != 0),]
  victim_dist <- victim[which(victim$victims == 1),]
  for(i in 2:max(victim$victims)){
    temp = victim[which(victim$victims == i),]
    if(dim(temp)[1]!=0){
      for(j in 1:i){
        victim_dist <- rbind(victim_dist, temp)
        print(paste("i:",i,"_j:",j,"_",dim(victim_dist)))
      }  
    }
  }
  print(paste("Time Use:",Sys.time()-t1))
  write_csv(victim_dist,"C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/data/total_victim.csv")
  
}

## read total victim data
victim_dist <- read_csv("C:/Users/54236/OneDrive/Documents/EDAV_final_car_collisions/Shiny/data/total_victim.csv")
## drop lon and lat columns with NA
victim_dist <- sf::st_as_sf(victim_dist[!is.na(victim_dist$lat),], coords = c("lon", "lat"))
victim_dist$VEHICLE.TYPE.CODE.1 <- as.factor(victim_dist$VEHICLE.TYPE.CODE.1)
victim_dist$CONTRIBUTING.FACTOR.VEHICLE.1 <- as.factor(victim_dist$CONTRIBUTING.FACTOR.VEHICLE.1)
attr(victim_dist[["geometry"]], "bbox") <- c(xmin = -74.257159, ymin = 40.495992, xmax = -73.699215,ymax= 40.915568)
## subset years
victim_dist2013 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2013-01-01" & victim_dist$ACCIDENT.DATE <= "2013-12-31",]
victim_dist2014 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2014-01-01" & victim_dist$ACCIDENT.DATE <= "2014-12-31",]
victim_dist2015 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2015-01-01" & victim_dist$ACCIDENT.DATE <= "2015-12-31",]
victim_dist2016 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2016-01-01" & victim_dist$ACCIDENT.DATE <= "2016-12-31",]
victim_dist2017 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2017-01-01" & victim_dist$ACCIDENT.DATE <= "2017-12-31",]
victim_dist2018 <- victim_dist[victim_dist$ACCIDENT.DATE >= "2018-01-01" & victim_dist$ACCIDENT.DATE <= "2018-12-31",]
  





