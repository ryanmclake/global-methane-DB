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
library(arrow)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

k <- read_csv("./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_add.csv") %>%
  select(lat, lon)

f <- read_csv("./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_add.csv") %>%
  select(-geometry) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

month_error <- f %>% group_by(num_months_sampled) %>%
  summarise(ebu_sd = sd(mean_ebu, na.rm = T),
            diff_sd = sd(mean_diff, na.rm = T),
            temp_sd = sd(mean_obs_wtemp_k, na.rm = T))

# h <- f %>% 
#   mutate(sd_ebu = ifelse(is.na(sd_ebu), num_months_sampled, sd_ebu)) %>%
#   mutate(sd_ebu = ifelse(sd_ebu == 1, 206.027380, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 2, 65.011561, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 3, 94.962037, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 4, 35.723655, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 5, 225.967137, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 6, 137.814703, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 7, 108.164537, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 8, 100.00000, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 9, 4.573157, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 10, 4.235644, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 11, 3.156525, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(sd_ebu == 12, 111.694275, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(is.na(sd_ebu), 100, sd_ebu))%>%
#   mutate(sd_ebu = ifelse(is.na(mean_ebu), NA, sd_ebu)) %>%
#   mutate(sd_diff = ifelse(is.na(sd_diff), num_months_sampled, sd_diff)) %>%
#   mutate(sd_diff = ifelse(sd_diff == 1, 122.733421, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 2, 47.944563, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 3, 56.540652, sd_diff))%>%  
#   mutate(sd_diff = ifelse(sd_diff == 4, 96.487207, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 5, 106.907952, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 6, 74.449700, sd_diff))%>%  
#   mutate(sd_diff = ifelse(sd_diff == 7, 187.249320, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 8, 10.164100, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 9, 3.662682, sd_diff))%>%  
#   mutate(sd_diff = ifelse(sd_diff == 10, 3.01254, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 11, 30.452968, sd_diff))%>%
#   mutate(sd_diff = ifelse(sd_diff == 12, 132.378793, sd_diff)) %>%
#   mutate(sd_diff = ifelse(is.na(sd_diff), 100, sd_diff)) %>%
#   mutate(sd_diff = ifelse(is.na(mean_diff), NA, sd_diff)) %>%
#   mutate(sd_obs_wtemp_k = ifelse(is.na(sd_obs_wtemp_k), num_months_sampled, sd_obs_wtemp_k)) %>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 1, 7.2956714, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 2, 4.8861203, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 3, 7.3251760, sd_obs_wtemp_k))%>%  
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 4, 7.4240255, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 5, 2.867429, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 6, 4.0654916, sd_obs_wtemp_k))%>%  
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 7, 2.7857509, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 8, 4.9157053, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 9, 5.1121841, sd_obs_wtemp_k))%>%  
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 10, 3.0129684, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 11, 0.9178777, sd_obs_wtemp_k))%>%
#   mutate(sd_obs_wtemp_k = ifelse(sd_obs_wtemp_k == 12, 7.5986401, sd_obs_wtemp_k)) %>%
#   mutate(sd_obs_wtemp_k = ifelse(is.na(sd_obs_wtemp_k), 5.5, sd_obs_wtemp_k)) %>%
#   mutate(sd_obs_wtemp_k = ifelse(is.na(mean_obs_wtemp_k), NA, sd_obs_wtemp_k))

# h <- f %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 0, 1, sd_ebu))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 2, 1.5, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 5, 3, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 10, 5, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 50, 10, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 100, 40, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 200, 80, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 500, 200, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 1000, 500, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 3000, 1600, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 5000, 2500, ebu_sd))) %>%
#   mutate(ebu_sd = ifelse(!is.na(sd_ebu), sd_ebu, ifelse(mean_ebu > 7000, 3000, ebu_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 0, 0.5, sd_diff))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 2, 0.8, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 5, 2, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 10, 3, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 50, 10, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 100, 30, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 200, 60, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 500, 100, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 1000, 300, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 3000, 500, diff_sd))) %>%
#   mutate(diff_sd = ifelse(!is.na(sd_diff), sd_diff, ifelse(mean_diff > 5000, 700, diff_sd))) %>%
#   select(-sd_ebu, -sd_diff)


h <- f %>%
  mutate(ebu_sd = ifelse(mean_ebu > 0, 1, sd_ebu)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 2, 1.5, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 5, 3, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 10, 5, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 50, 10, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 100, 40, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 200, 80, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 500, 200, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 1000, 500, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 3000, 1600, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 5000, 2500, ebu_sd)) %>%
  mutate(ebu_sd = ifelse(mean_ebu > 7000, 3000, ebu_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 0, 0.5, sd_diff)) %>%
  mutate(diff_sd = ifelse(mean_diff > 2, 0.8, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 5, 2, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 10, 3, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 50, 10, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 100, 30, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 200, 60, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 500, 100, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 1000, 300, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 3000, 500, diff_sd)) %>%
  mutate(diff_sd = ifelse(mean_diff > 5000, 700, diff_sd)) %>%
  select(-sd_ebu, -sd_diff)
  
bind_cols(h, k) %>% write_csv(., "./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_sd_added.csv")


obs_diffusive_flux <- h %>% select(mean_diff, diff_sd, waterbody_type) %>% na.omit(.) %>%
  ggplot() +
  geom_sf(data = world, lwd = 0.3, color = "black", fill = "white")+
  xlab("Longitude") + ylab("Latitude") +
  labs(title = paste0("    N diffusive observations = 1208"))+
  geom_sf(data = h, pch = 21, color = "black", aes(size = mean_diff, fill = diff_sd))+
  scale_size("Methane Diffusion Rate")+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="dodgerblue4",
                       high="red", space ="Lab", na.value="grey",
                       name = "Standard Deviation") +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.5),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

obs_ebullition_flux <- h %>% select(mean_ebu, ebu_sd) %>% na.omit(.) %>%
  ggplot() +
  geom_sf(data = world, lwd = 0.3, color = "black", fill = "white")+
  xlab("Longitude") + ylab("Latitude") +
  labs(title = paste0("    N diffusive observations = 614"))+
  geom_sf(data = h, pch = 21, color = "black", aes(size = mean_ebu, fill = ebu_sd))+
  scale_size("Methane Ebullition Rate")+
  scale_fill_gradient2(midpoint=0, low="tan1", mid="dodgerblue4",
                       high="red", space ="Lab", na.value="grey",
                       name = "Standard Deviation") +
  guides(fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.5),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))
