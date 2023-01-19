library(dplyr)
library(vroom)
library(readr)
library(ggplot2)
library(patchwork)
library(sf)
library(units)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(tidyverse)

library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

d <- vroom::vroom("./data/organized_data_to_append/global_lake_res_DB.csv")
points = data.frame(lon=c(d$lon), lat=c(d$lat))

countries <- data.frame(coords2country(points)) %>%
  write_csv("./data/countries.csv")

length(unique(coords2country(points)))

# Load in the dataset
d1_diff <- vroom::vroom("./data/organized_data_to_append/global_lake_res_DB.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>%
  select(ch4_diff, geometry) %>%
  na.omit(.)

d1_ebu <- vroom::vroom("./data/organized_data_to_append/global_lake_res_DB.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>%
  select(ch4_ebu, geometry) %>%
  na.omit(.)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

obs_diffusive_flux <- 
  ggplot() +
  geom_sf(data = world, lwd = 0.3, color = "black", fill = "white")+
  xlab("Longitude") + ylab("Latitude") +
  labs(title = paste0("    N diffusive observations = ", length(d1_diff$ch4_diff)))+
  geom_sf(data = d1_diff, pch = 21, size = 3, color = "black", aes(fill = sqrt(ch4_diff)))+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="dodgerblue4",
                       high="red", space ="Lab", na.value="grey",
                       name = "**Diffusive Methane Emissions** <br>√(mgCH4/m2*d)") +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.5),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

obs_ebullition_flux <- 
  ggplot() +
  geom_sf(data = world, lwd = 0.3, color = "black", fill = "white")+
  xlab("Longitude") + ylab("Latitude") +
  labs(title = paste0("    N ebullition observations = ", length(d1_ebu$ch4_ebu)))+
  geom_sf(data = d1_ebu, pch = 21, size = 3, color = "black", aes(fill = sqrt(ch4_ebu)))+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="dodgerblue4",
                       high="red", space ="Lab", na.value="grey",
                       name = "**Ebullitive Methane Emissions** <br>√(mgCH4/m2*d)") +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.5),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))
