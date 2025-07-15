## This script constructs initial plots to explore carbonate chemistry in Sequim
## Bay based off of Jess Cross's LDRD dataset
##
## 2025-06-30
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")

df <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250701_carbonate_chem_data_Cross.csv")


b = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df))[[4]][1,1]
m = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df))[[4]][2,1]
r2 = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df))[[9]]

df %>% 
  ggplot(aes(salinity_practical_salinity_scale, ta_umol_kg_20)) + 
  geom_point(color = "gray", size = 3, aes(shape = station_name)) + 
  geom_point(size = 2.2, aes(color = tidal_name, shape = station_name)) + 
  #geom_smooth(se = F, color = "black") + 
  geom_smooth(method = "lm", se = F, color = "black", linetype = "dashed") + 
  scale_color_viridis_d() + 
  #scale_alpha_continuous(range = c(0.4, 1)) + 
  labs(x = "Salinity (PSU)", y = "Total alkalinity (umol/kg)", 
       color = "Tide", shape = "Location", 
       title = "1 value removed (TA = 2301)") + 
  annotate(geom = "text", x = 31, y = 2190, label = paste0("R2=", round(r2, 2))) + 
  annotate(geom = "text", x = 31, y = 2200, label = paste0("TA=", round(m, 0), "*S+", round(b, 0))) 
ggsave("figures/250702_sal_vs_ta.png", width = 5, height = 4)


df %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(dic_umol_kg_20_c, ta_umol_kg_20)) + 
  geom_point(color = "gray", size = 3, aes(shape = station_name)) + 
  geom_point(size = 2.2, aes(color = tidal_name, shape = station_name)) + 
  scale_color_viridis_d() + 
  labs(x = "Dissolved inorganic carbon (umol/kg)", 
       y = "Total alkalinity (umol/kg)", 
       color = "Tide", shape = "Location")
ggsave("figures/250702_dic_vs_ta.png", width = 5, height = 4)


## Estimate salinity!

ctd <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv")

ta_estimates <- ctd %>% 
  mutate(est_ta = b + salinity_psu_clean*m) %>% 
  mutate(doy = yday(time_pst)) %>% 
  group_by(doy) %>% 
  summarize(across(all_of(c("est_ta")), 
                   list(max = max, mean = mean, min = min), na.rm = TRUE)) %>% 
  complete(doy = 1:366) 

p1 <- ctd %>% 
  assign_season() %>% 
  mutate(est_ta = b + salinity_psu_clean*m) %>% 
  ggplot(aes(season, est_ta, fill = season)) + 
  geom_boxplot(alpha = 0.5, show.legend = F) + 
  scale_fill_viridis_d()

p2 <- ggplot(ta_estimates %>% drop_na(), aes(doy)) + 
  geom_ribbon(aes(ymin = est_ta_min, 
                  ymax = est_ta_max), 
              alpha = 0.4, fill = "red", na.rm = T) + 
  geom_smooth(aes(y = est_ta_mean), lwd = 1, color = "black") + 
  labs(x = "Day of Year", y = "Estimated TA (umol/kg)")

plot_grid(p1, p2)
ggsave("figures/250714_est_ta.png", width = 8, height = 4)







