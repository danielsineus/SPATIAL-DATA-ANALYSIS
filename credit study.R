library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(raster)
library(sp)

getwd()
# the spatial class has 10 subclasses
getClass("Spatial")
getClass("SpatialPolygonsDataFrame")
#import the data 
kiva<-read.csv("kivaData_augmented.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(kiva)
dim(kiva)
mpidata<-read.csv("MPIData_augmented.csv", header = TRUE)
colnames(mpidata)
kiva_data<-kiva[,c(1:12,22:24)]
colnames(kiva_data)
kiva1<-kiva_data%>%
  drop_na()
dim(kiva1)
coordinates(kiva1)<-c("longitude", "latitude")
# the class is spatialpointsdataframe and sp
str(kiva1)
summary(kiva1)
#we notice that proj4string is NA
# the coordinate reference system is missing
crs.geo1<-CRS("+proj=longlat")
proj4string(kiva1)<-crs.geo1
summary(kiva1)

dim(kiva1)
library(rgdal)
#write and save the data in shapefile
writeOGR(kiva1, dsn = "C:/Users/Sineus/Desktop/spatial data analysis",layer="kiva1", driver = "ESRI Shapefile")

#look for the Directory
dir()
#read the data
loandata<-readOGR("C:/Users/Sineus/Desktop/spatial data analysis", "kiva1", verbose = TRUE)
summary(loandata)

summary(kiva1)
summary(loandata)
library(tmap)
head(loandata@data$lon_mnt)
tm_shape(loandata) +
  tm_fill(col = "lon_mnt")+
 
tm_shape(coordinates(loandata))+
  tm_fill("lon_mnt")

plot(kiva1)
plot(coordinates(kiva1), col="red", add=TRUE)
class(loandata)

isn<-loandata$sector=="Education"
reda<-loandata[isn,]
plot(reda)
library(sp)
library(tmap)
qtm(shp = loandata, fill = "lon_mnt")

sector_activity<-kiva_data%>%
  select(activity,sector, loan_amount)%>%
  group_by(sector, activity)%>%
  summarise(tota=sum(loan_amount),
            som=n(),
            aver=mean(loan_amount, na.rm=TRUE))%>%
  arrange(!desc(sector))

head(sector_activity)
loan_sector<-kiva_data%>%
  group_by(sector)%>%
  summarise(tota=sum(loan_amount))%>%
  arrange(desc(tota))

head(loan_sector)

sector_Education<-kiva_data[kiva_data$sector=="Education",]
head(sector_Education,5)

money_sect<-kiva_data%>%
  filter(sector=="Education")%>%
  group_by(country)%>%
  summarise(tota=sum(loan_amount))%>%
  arrange(desc(tota))

head(money_sect,5)

educ_ag<-aggregate(loan_amount~country, FUN = sum, data = sector_Education)
head(educ_ag)
loan_country<-kiva_data%>%
  group_by(country, longitude, latitude)%>%
  summarise(tot=sum(loan_amount,na.rm=TRUE))%>%
  drop_na()

head(loan_country)

dim(loan_country)


glimpse(kiva_data)

class(loan_country)
library(RColorBrewer)
help("RColorBrewer")
help("hue")
library(rworldmap)

world<-getMap(resolution = "low")
class(world)
plot(world)
str(world@data)

(with_world<-ggplot()+
    geom_polygon(data = world@data, aes(x=LON, y=LAT, group=REGION))+
    geom_point(data=loan_country,
               aes(x=longitude, y=latitude,
                   colour="red"))+
  coord_quickmap()+
    theme_classic()+
    xlab("longitude")+
    ylab("latitude")+
    guides(colour=guide_legend(title = "loan")))

plot(world)

aggricult<-kiva_data%>%
  filter(sector=="Agriculture")
head(aggricult)

attach(kiva_data)
library(ggmap)
map_world<-get_map(location="world", zoom=5, scale=1)
class(map_world)
ggplot(data=kiva_data, aes(x=longitude, y=latitude, group=country, fill=sector))+
  geom_polygon(color="white")+
  coord_map()
ggmap(map_world)+
  geom_point(data = kiva_data, aes(x=longitude, y=latitude), color="red")
str(map_world)
ggmap(aggricult)%>%
  ggplot()

print(countries_sp)


# Coordinates for the location of interest
nyc <- c(lon = -74.0059, lat = 40.7128)
str(nyc)
# 1. Download the relevant map
nyc_map <- get_map(location = nyc, zoom = 10)
# 2. Display the map
ggmap(nyc_map)
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}
points = data.frame(lon=c(0, 90, -45, -100, 130), lat=c(52, 40, -10, 45, -30 ))

coords2continent(points)
#[1] Europe        Asia          South America North America Australia  
coords2country(points)
wrod<-get_map("world")%>%ggmap()

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()

install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "googleway", "ggspatial", "cowplot"))
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
world<-ne_countries(scale="medium", returnclass = "sf")
class(world)
str(world)
head(world)
 
  
gplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'Countries with highest "talent competitiveness"'
       ,subtitle = "source: INSEAD, https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos") +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )



