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


