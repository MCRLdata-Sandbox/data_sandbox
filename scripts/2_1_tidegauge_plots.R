## This script explores tidal, diurnal, seasonal, and annual patterns in water 
## level data from the cleaned-up tidegauge dataset
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")

df <- read_csv("data/mcrl_data/250515_tidegauge_L1.csv") %>% 
  assign_season()



p1 <- assign_season(df) %>% 
  mutate(tod = hour(as_hms(time_pst))) %>% 
  group_by(date, tod) %>% 
  summarize(season = first(season), 
            water_level_m_navd88 = mean(water_level_m_navd88)) %>% 
  ggplot(aes(tod, water_level_m_navd88)) + 
  geom_line(aes(group = date), color = "gray", alpha = 0.5) + 
  geom_vline(xintercept = 12, linetype = "dashed") + 
  geom_smooth() + 
  facet_wrap(~season) + 
  labs(x = "Water level (m NAVD88)", y = "Hour of day")



