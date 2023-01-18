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
