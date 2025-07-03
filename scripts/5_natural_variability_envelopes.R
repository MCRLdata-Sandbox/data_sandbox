## This script calculates natural variability envelopes for Sequim Bay for 
## parameters of interest, exported both as figures and as tables with summary
## statistics
##
## 2025-07-02
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")


# 2. Load data -----------------------------------------------------------------

ctd <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv") %>% 
  assign_season()

ctd %>% 
  ggplot(aes(month, temp_deg_c, fill = season)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  scale_fill_viridis_d() + 
  labs(x = "", y = "Water temperature")

ctd %>% 
  assign_season() %>% 
  ggplot(aes(month, salinity_psu_clean, fill = season)) + 
  geom_boxplot(show.legend = F, alpha = 0.5) + 
  scale_fill_viridis_d() + 
  labs(x = "", y = "Salinity")

ctd %>% 
  assign_season() %>% 
  mutate(doy = yday(time_pst)) %>% 
  group_by(doy) %>% 
  summarize(mean = mean(temp_deg_c, na.rm = T), 
            sd = sd(temp_deg_c, na.rm = T)) %>% 
  ggplot(aes(x = doy)) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) + 
  geom_smooth(aes(y = mean), se = F) + 
  scale_fill_viridis_d() + 
  labs(x = "", y = "Salinity")

