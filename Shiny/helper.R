## set up
library(readr)
library(tidyr)
library(tidyverse)
library(extracat)
library(ggmap)
keys = 'AIzaSyDinTb_yuI0U1VPGq6UBv1_O_Kr-PO8Kn4'
register_google(key = keys)

#read clean data
df2016_2018=read.csv("data/data2016-2018.csv", header = T, na.string = c("", "NA"), sep = ",")
# clean data
df2016_2018rm = df2016_2018[complete.cases(df2016_2018$LATITUDE), ]
df2016_2018rm = df2016_2018rm[as.numeric(df2016_2018rm$LONGITUDE) > -74.4, ]
df2016_2018rm = df2016_2018rm[as.numeric(df2016_2018rm$LONGITUDE)< -73.6, ]
df2016_2018rm = df2016_2018rm[as.numeric(df2016_2018rm$LATITUDE)< 41, ]
df2016_2018rm = df2016_2018rm[as.numeric(df2016_2018rm$LATITUDE) > 40.4, ]
df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1 = toupper(df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1)

newyork = ggmap(get_googlemap(center = c(lon = -74.006266, lat = 40.7242551),
                          zoom = 11, scale = 2, 
                          maptype ='roadmap',
                          color = 'color')) + coord_equal()

brooklyn = ggmap(get_googlemap(center = c(lon = -73.949997, lat = 40.650002),
                    zoom = 12, scale = 2, 
                    maptype ='roadmap',
                    color = 'color')) + coord_equal()

bronx = ggmap(get_googlemap(center = c(lon = -73.865433, lat = 40.837048),
                    zoom = 12, scale = 2, 
                    maptype ='roadmap',
                    color = 'color')) + coord_equal()

queens = ggmap(get_googlemap(center = c(lon = -73.769417, lat = 40.742054),
                    zoom = 12, scale = 2, 
                    maptype ='roadmap',
                    color = 'color')) + coord_equal()
island = ggmap(get_googlemap(center = c(lon = -74.151535, lat = 40.579021),
                    zoom = 12, scale = 2, 
                    maptype ='roadmap',
                    color = 'color')) + coord_equal()
manhattan = ggmap(get_googlemap(center = c(lon = -73.985130, lat = 40.758896),
                    zoom = 12, scale = 2, 
                    maptype ='roadmap',
                    color = 'color')) + coord_equal()

df2016_2018rm_kill = mutate(df2016_2018rm, NUMBER.OF.PERSONS.KILLED = ifelse(is.na(NUMBER.OF.PERSONS.KILLED), 0,as.numeric(NUMBER.OF.PERSONS.KILLED)))
df_pkillnozero = filter(df2016_2018rm_kill, df2016_2018rm$NUMBER.OF.PERSONS.KILLED != 0)

df2016_2018rm_inju = mutate(df2016_2018rm, NUMBER.OF.PERSONS.INJURED = ifelse(is.na(NUMBER.OF.PERSONS.INJURED), 0,as.numeric(NUMBER.OF.PERSONS.INJURED)))
df_pinnozero = filter(df2016_2018rm_inju, df2016_2018rm$NUMBER.OF.PERSONS.INJURED != 0)


data2016 = df2016_2018rm[which(df2016_2018rm$year == 2016),]
data2017 = df2016_2018rm[which(df2016_2018rm$year == 2017),]
data2018 = df2016_2018rm[which(df2016_2018rm$year == 2018),]

df_pinnozero2016 = df_pinnozero[which(df_pinnozero$year == 2016),]
df_pinnozero2017 = df_pinnozero[which(df_pinnozero$year == 2017),]
df_pinnozero2018 = df_pinnozero[which(df_pinnozero$year == 2018),]

df_pkillnozero2016 = df_pkillnozero[which(df_pkillnozero$year == 2016),]
df_pkillnozero2017 = df_pkillnozero[which(df_pkillnozero$year == 2017),]
df_pkillnozero2018 = df_pkillnozero[which(df_pkillnozero$year == 2018),]
  
alco = df2016_2018rm[df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1=='ALCOHOL INVOLVEMENT',]
back = df2016_2018rm[df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1=='BACKING UNSAFELY',]
distr = df2016_2018rm[df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1=='DRIVER INATTENTION/DISTRACTION',]
fail = df2016_2018rm[df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1=='FAILURE TO YIELD RIGHT-OF-WAY',]
foll = df2016_2018rm[df2016_2018rm$CONTRIBUTING.FACTOR.VEHICLE.1=='FOLLOWING TOO CLOSELY',]