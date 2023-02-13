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

d <- vroom::vroom("./data/organized_data_to_append/global_lake_res_CH4_emission_DB_all.csv")

length(unique(d$ref))

duplicates <- d[duplicated(d$ref) | duplicated(d$ref, fromLast = TRUE), ]

length(unique(duplicates$ref))

repeats <- unique(duplicates$ref)

data_wo_repeats <- d %>% filter(!ref %in% repeats)
data_w_repeats <- d %>% filter(ref %in% repeats)

johnson <- data_w_repeats %>% filter(data_source == "Johnson et al 2021") %>% select(lat, lon, ch4_diff, ch4_ebu, ref, data_source, obs_year) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

prairie <- data_w_repeats %>% filter(data_source == "Prairie et al 2018") %>% select(lat, lon, ch4_diff, ch4_ebu, ref, data_source, obs_year) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

rosentreter <- data_w_repeats %>% filter(data_source == "Rosentreter et al 2020") %>% select(lat, lon, ch4_diff, ch4_ebu, ref, data_source, obs_year) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")

kuhn <- data_w_repeats %>% filter(data_source == "Kuhn et al 2021") %>% select(lat, lon, ch4_diff, ch4_ebu, ref, data_source, obs_year) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=eqearth +wktext")


# Link Johnson to Prairie

link1 <- johnson %>%
  cbind(prairie[st_nearest_feature(johnson, prairie),]) %>%
  mutate(dist = (st_distance(geometry, geometry.1, by_element = T))*0.001) %>%
  st_drop_geometry(.) %>%
  arrange(dist) %>%
  filter(ref == ref.1)

compare_J_and_P_diff <- link1 %>% select(ch4_diff, data_source, ch4_diff.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_diff.1 == 0, NA, ch4_diff.1)) %>%
  na.omit(.) %>%
  ggplot(., aes(x = ch4_diff.1, y = ch4_diff, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Johnson et al 2021 - Diffusion")+
  xlab("Prairie et al 2018 - Diffusion")+
  labs(title = "Johnson vs. Prairie - Diffusion")

# Link Johnson to Rosentreter

link2 <- johnson %>%
  cbind(rosentreter[st_nearest_feature(johnson, rosentreter),]) %>%
  mutate(dist = (st_distance(geometry, geometry.1, by_element = T))*0.001) %>%
  st_drop_geometry(.) %>%
  arrange(dist) %>%
  filter(ref == ref.1)

compare_J_and_R_diff <- link2 %>% select(ch4_diff, data_source, ch4_diff.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_diff.1 == 0, NA, ch4_diff.1)) %>%
  na.omit(.) %>%
  ggplot(., aes(x = ch4_diff.1, y = ch4_diff, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Johnson et al 2021 - Diffusion")+
  xlab("Rosentreter et al 2020 - Diffusion")+
  labs(title = "Johnson vs. Rosentreter - Diffusion")

compare_J_and_R_ebu <- link2 %>% select(ch4_ebu, data_source, ch4_ebu.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_ebu.1 == 0, NA, ch4_ebu.1)) %>%
  na.omit(.) %>%
  ggplot(., aes(x = ch4_ebu.1, y = ch4_ebu, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Johnson et al 2021 - Ebullition")+
  xlab("Rosentreter et al 2020 - Ebullition")+
  labs(title = "Johnson vs. Rosentreter - Ebullition")

# Link Johnson to Kuhn

link3 <- johnson %>%
  cbind(kuhn[st_nearest_feature(johnson, kuhn),]) %>%
  mutate(dist = (st_distance(geometry, geometry.1, by_element = T))*0.001) %>%
  st_drop_geometry(.) %>%
  arrange(dist) %>%
  filter(ref == ref.1)

compare_J_and_K_diff <- link3 %>% select(ch4_diff, data_source, ch4_diff.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_diff.1 == 0, NA, ch4_diff.1)) %>%
  na.omit(.) %>%
  ggplot(., aes(x = ch4_diff.1, y = ch4_diff, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Johnson et al 2021 - Diffusion")+
  xlab("Kuhn et al 2020 - Diffusion")+
  labs(title = "Johnson vs. Kuhn - Diffusion")

compare_J_and_K_ebu <- link3 %>% select(ch4_ebu, data_source, ch4_ebu.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_ebu.1 == 0, NA, ch4_ebu.1)) %>%
  na.omit(.) %>%
  ggplot(., aes(x = ch4_ebu.1, y = ch4_ebu, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Johnson et al 2021 - Ebullition")+
  xlab("Kuhn et al 2020 - Ebullition")+
  labs(title = "Johnson vs. Kuhn - Ebullition")

# Link Rosentreter to Kuhn

link4 <- rosentreter %>%
  cbind(kuhn[st_nearest_feature(rosentreter, kuhn),]) %>%
  mutate(dist = (st_distance(geometry, geometry.1, by_element = T))*0.001) %>%
  st_drop_geometry(.) %>%
  arrange(dist) %>%
  filter(ref == ref.1)

compare_R_and_K_diff <- link4 %>% select(ch4_diff, data_source, ch4_diff.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_diff.1 == 0, NA, ch4_diff.1)) %>%
  ggplot(., aes(x = ch4_diff.1, y = ch4_diff, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Rosentreter et al 2021 - Diffusion")+
  xlab("Kuhn et al 2020 - Diffusion")+
  labs(title = "Rosentreter vs. Kuhn - Diffusion")

compare_R_and_K_ebu <- link4 %>% select(ch4_ebu, data_source, ch4_ebu.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_ebu.1 == 0, NA, ch4_ebu.1)) %>%
  ggplot(., aes(x = ch4_ebu.1, y = ch4_ebu, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Rosentreter et al 2021 - Ebullition")+
  xlab("Kuhn et al 2020 - Ebullition")+
  labs(title = "Rosentreter vs. Kuhn - Ebullition")

# Link Rosentreter to Prairie

link5 <- rosentreter %>%
  cbind(prairie[st_nearest_feature(rosentreter, prairie),]) %>%
  mutate(dist = (st_distance(geometry, geometry.1, by_element = T))*0.001) %>%
  st_drop_geometry(.) %>%
  arrange(dist) %>%
  filter(ref == ref.1)

compare_R_and_P_diff <- link5 %>% select(ch4_diff, data_source, ch4_diff.1, data_source.1, ref) %>% 
  mutate(ch4_diff.1 = ifelse(ch4_diff.1 == 0, NA, ch4_diff.1)) %>%
  ggplot(., aes(x = ch4_diff.1, y = ch4_diff, color = ref))+
  geom_point(alpha = 1, size = 4, pch = 19)+
  theme_classic()+
  #geom_smooth(method = lm, se = T, alpha = 0.1)+
  geom_abline(slope = 1)+
  ylab("Rosentreter et al 2021 - Diffusion")+
  xlab("Prairie et al 2020 - Diffusion")+
  labs(title = "Rosentreter vs.Prairie - Diffusion")

# NO MAKJOR REPEATS IN THE LINK FROM Prairie to Kuhn





