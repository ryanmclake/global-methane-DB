#### Libraries #### 
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)
library(sf, warn.conflicts = FALSE)
library(units, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(doParallel, warn.conflicts = FALSE)

s = Sys.time()

i <- read_csv("./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_sd_added.csv") %>%
  mutate(country = ifelse(country == "French Guiana", "France", country))
glee_countries <- c(unique(i$country))

countries <- list.files(path = "./data/glcp_countries/")
countries <- gsub("\\..*", "", countries)

#paste0("./countries/",country[1],".csv"),   


#### Import, select, summarize, export ####
glcp_bind_function <- function(x){

glee_country <- read_csv("./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_sd_added.csv") %>% 
    filter(country == x) %>% select(-geometry) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform("+proj=eqearth +wktext") %>%
    rename(year = obs_year)

lat_lon <- read_csv("./data/organized_data_to_append/global_lake_res_DB_combined_refs_WWF_sd_added.csv") %>% 
  filter(country == x) %>% select(lat, lon) 
  
glcp_country <- read_csv_arrow(paste0("./data/glcp_countries/",x,".csv"),
                    quote = "\"",
                    escape_double = TRUE,
                    escape_backslash = FALSE,
                    schema = NULL,
                    col_names = F,
                    col_types = NULL,
                    col_select = NULL,
                    na = c("", "NA"),
                    quoted_na = TRUE,
                    skip_empty_rows = TRUE,
                    skip = 0L,
                    parse_options = NULL,
                    convert_options = NULL,
                    read_options = NULL,
                    as_data_frame = TRUE,
                    timestamp_parsers = NULL) %>% rename(year = f0,
                                                         month=f1,
                                                         hylak_id=f2,
                                                         centr_lat=f3,
                                                         centr_lon=f4,
                                                         continent=f5,
                                                         country=f6,
                                                         bsn_lvl=f7,
                                                         hybas_id=f8,
                                                         mean_monthly_precip_mm=f9,
                                                         total_precip_mm=f10,
                                                         mean_annual_temp_k=f11,
                                                         pop_sum=f12,
                                                         seasonal_km2=f13,
                                                         permanent_km2=f14,
                                                         total_km2=f15,
                                                         lake_name=f16,
                                                         lake_type=f17,
                                                         lake_area=f18,
                                                         shore_dev=f19,
                                                         vol_total=f20,
                                                         vol_res=f21,
                                                         vol_src=f22,
                                                         depth_avg=f23,
                                                         res_time=f24,
                                                         elevation=f25,
                                                         slope_100=f26,
                                                         wshd_area=f27,
                                                         pour_long=f28,
                                                         pour_lat=f29,
                                                         sub_area=f30,
                                                         mean_spec_humidity=f31,
                                                         mean_precip_mm=f32,
                                                         sum_precip_mm=f33,
                                                         mean_temp_k=f34,
                                                         mean_totcloud_pct=f35,
                                                         mean_sw_wm2=f36,
                                                         mean_lw_wm2=f37,
                                                         above_ratio_cutoff=f38,
                                                         ice_cover_min=f39,
                                                         ice_cover_max=f40,
                                                         ice_cover_mean=f41,
                                                         ice_cover_median=f42,
                                                         ice_cover_binary_min=f43,
                                                         ice_cover_binary_max=f44,
                                                         ice_cover_binary_mean=f45,
                                                         ice_cover_binary_median=f46,
                                                         ice_cover_count=f47,
                                                         snow_km2=f48) %>%
  mutate(horizontal_radiance = mean_sw_wm2 * 730 / 1000 / 30)%>%
  select(year, 
         hylak_id,
         hybas_id, 
         centr_lat, 
         centr_lon, 
         elevation, 
         depth_avg, 
         ice_cover_mean,
         mean_temp_k, 
         horizontal_radiance,
         pop_sum,
         total_precip_mm,
         mean_sw_wm2, 
         ice_cover_mean)%>%
  group_by(hylak_id, hybas_id, year, centr_lat, centr_lon, elevation) %>%
  summarize(glcp_ice_months = length(year[mean_temp_k < 273.15]),
            glcp_mean_annual_temp = mean(mean_temp_k, na.rm = T),
            glcp_mean_annual_temp_sd = sd(mean_temp_k, na.rm = T),
            glcp_annual_horizontal_radiance = mean(horizontal_radiance, na.rm = T),
            glcp_annual_horizontal_radiance_sd = sd(horizontal_radiance, na.rm = T),
            glcp_pop_sum = mean(pop_sum, na.rm = T),
            glcp_ice_cover_mean = mean(ice_cover_mean, na.rm = T),
            glcp_mean_sw_wm2 = mean(mean_sw_wm2, na.rm = T),
            glcp_mean_sw_wm2_sd = sd(mean_sw_wm2, na.rm = T),
            glcp_total_precip_mm = mean(total_precip_mm, na.rm = T))%>%
  collect() %>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  #projecting
  st_transform("+proj=eqearth +wktext")

glcp_glee_join <- do.call('rbind', lapply(split(glee_country, 1:nrow(glee_country)), function(x) {
  st_join(x, glcp_country[glcp_country$year == unique(x$year),], join = st_nearest_feature)
})) %>% bind_cols(., lat_lon) %>% st_drop_geometry() %>%
  write.table(., file = paste0("./data/organized_data_to_append/global_lake_res_DB_refs_WWF_GLCP.txt"),
            append = T,
            row.names = T,
            col.names = !file.exists("./data/organized_data_to_append/global_lake_res_DB_refs_WWF_GLCP.txt"))
}

no_cores <- detectCores() - 5
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
foreach(x=countries) %dopar% glcp_bind_function(x)

#### Time check ####
e <- Sys.time()
t=e-s
print(t)


