

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,viridis)

temp_ebu_function_johnson <- function(x) {(100 * 1.1 ^ (x - 20))}
temp_ebu_function_mcclure_mean <- function(x) {(125 * 1.3 ^ (x - 20))}
temp_ebu_function_mcclure_min <- function(x) {(125 * 1.2 ^ (x - 20))}
temp_ebu_function_mcclure_max <- function(x) {(125 * 1.4 ^ (x - 20))}

ebu_function_output <- data.frame(x = -10:40,            # Create data for ggplot2
                                  values = c(temp_ebu_function_johnson(-10:40),
                                             temp_ebu_function_mcclure_mean(-10:40),
                                             temp_ebu_function_mcclure_min(-10:40),
                                             temp_ebu_function_mcclure_max(-10:40)),
                                  model = rep(c("Johnson",
                                                "Bayes - Mean",
                                                "Bayes - Min",
                                                "Bayes - Max"), each = 51))


w <- read.table("./data/organized_data_to_append/global_lake_res_DB_refs_WWF_GLCP.txt", header = TRUE, row.names = NULL) %>%
  select(mean_diff, mean_ebu, ebu_sd, diff_sd, glcp_mean_annual_temp, mean_obs_wtemp_k, lat) %>%
  mutate(temp_for_model = ifelse(is.na(glcp_mean_annual_temp),mean_obs_wtemp_k, glcp_mean_annual_temp))


ebu <- w %>% select(mean_ebu, ebu_sd, temp_for_model, lat) %>% na.omit(.) %>% filter(mean_ebu > 0) %>%
  mutate(temp_for_model = temp_for_model - 273.15)

ggplot(ebu) +
  geom_point(aes(temp_for_model, mean_ebu)) +
  geom_line(data = ebu_function_output, aes(x, values, group = model, color = model))+
  scale_color_viridis(discrete = T, option = "C")+
  ylim(c(0,7000))




diff <- w %>% select(mean_diff, diff_sd, temp_for_model, lat) %>% na.omit(.) %>% filter(mean_diff > 0)






w %>% select(mean_diff, temp_for_model, lat) %>%
  mutate(temp_for_model = temp_for_model-273.15) %>%
  filter(mean_diff < 5000) %>%
  ggplot(.) +
  geom_point(aes(temp_for_model, mean_diff, color = lat)) +
  geom_line(data = diff_function_johnson, aes(x, values))
  

w %>% select(mean_ebu, temp_for_model, lat) %>%
  mutate(temp_for_model = temp_for_model-273.15) %>%
  




w %>% select(mean_diff, temp_for_model, lat) %>%
  filter(lat < 45) %>%
  filter(lat > 0) %>%
  ggplot(.) +
  geom_point(aes(temp_for_model, mean_diff, color = lat))



w %>% select(mean_diff, temp_for_model, lat) %>%
  filter(lat < 0) %>%
  ggplot(.) +
  geom_point(aes(temp_for_model, mean_diff, color = lat))






w %>% select(mean_diff, temp_for_model, lat) %>%
  filter(lat < -45) %>%
  ggplot(.) +
  geom_point(aes(temp_for_model, mean_diff, color = lat))
