library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
kiva<-read.csv("kivaData_augmented.csv", header = TRUE)
colnames(kiva)
dim(kiva)
mpidata<-read.csv("MPIData_augmented.csv", header = TRUE)
colnames(mpidata)
str(kiva)
kiva_data<-kiva[1:13]
dim(kiva_data)
str(kiva_data)
head(kiva_data,5)
loan_sector<-kiva_data%>%
  select(country, sector, activity, loan_amount)%>%
  group_by(sector)%>%
  summarise(tota=sum(loan_amount),
            aver=mean(loan_amount, na.rm=TRUE))%>%
  arrange(desc(tota))

head(loan_sector)

loan_country<-kiva_data%>%
  group_by(country, longitude, latitude)%>%
  summarise(tot=sum(loan_amount,na.rm=TRUE))%>%
  drop_na()

head(loan_country)
dim(loan_country)
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

#Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)
#set the API Key
ggmap::register_google(key = "AIzaSyDIWFZWSD82HBfQGqwKIjlsm4wR43-y0lk")
word_data<-map_data("world")
str(word_data)


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