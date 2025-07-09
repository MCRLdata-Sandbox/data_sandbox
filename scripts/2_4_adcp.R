## This script creates figures for air temperature based on L1 data created. It
## Will also pull in water temperatures, and potentially explore the relationships
## between these parameters
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

p_load(WaveletComp)

adcp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250703_adcp_velocity_L1.csv") %>% 
  assign_season()

tidegauge <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv")

df <- left_join(adcp, tidegauge, by = "time_pst")

df_doy <- df %>% 
  mutate(doy = yday(time_pst)) %>% 
  group_by(doy) %>% 
  summarize(mean = mean(max_velocity_m_s, na.rm = T), 
            sd = sd(max_velocity_m_s, na.rm = T))

max_v = round(max(df$max_velocity_m_s, na.rm = T), 1)
median_v = round(median(df$max_velocity_m_s, na.rm = T), 1)

plot_grid(ggplot(df_doy, aes(doy)) + 
            geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) + 
            geom_smooth(aes(y = mean), se = F, color = "black", span = 0.15) + 
            labs(x = "Day of year", y = "Water velocity (m/s)"), 
          df %>% 
            ggplot(aes(water_level_m_navd88, max_velocity_m_s)) + 
            geom_point(alpha = 0.02) + 
            labs(x = "Water level (m NAVD88)", 
                 y = "Water velocity (m/s)") + 
            annotate(geom = "text", x = 1.5, y = 2.5, label = paste0("Max: ", max_v, " m/s")) + 
            annotate(geom = "text", x = 1.5, y = 2.2, label = paste0("Median: ", median_v, " m/s")),
          nrow = 1, rel_widths = c(1, 0.7))
ggsave("figures/250709_adcp.png", width = 8, height = 3.5)



x <- df %>% 
  drop_na(max_velocity_m_s, water_level_m_navd88) %>% 
  filter(time_pst > "2021-10-01" &
           time_pst < "2022-02-13") %>%
  # rename("wlm" = water_level_m_navd88, 
  #        "velms" = max_velocity_m_s,
  #        "timepst" = time_pst) %>% 
  dplyr::select(time_pst, water_level_m_navd88, max_velocity_m_s) %>% 
  mutate(v_lag5 = lag(max_velocity_m_s, 5), 
         v_lead5 = lead(max_velocity_m_s, 5)) %>% 
  mutate(water_level_smoothed = rollmean(water_level_m_navd88, 
                                         k = 5, fill = NA, align = "center")) %>% 
  mutate(tide_limb = case_when(
    water_level_smoothed > lag(water_level_smoothed) ~ "rising",
    water_level_smoothed < lag(water_level_smoothed) ~ "falling",
    water_level_smoothed == lag(water_level_smoothed) ~ "slack",
    TRUE ~ NA_character_))

ccf(x$water_level_m_navd88, x$max_velocity_m_s)

summary(lm(max_velocity_m_s~water_level_m_navd88, data = x))
summary(lm(v_lag5~water_level_m_navd88, data = x))

as_tibble(x) %>% 
  filter(tide_limb != "NA") %>% 
  ggplot(aes(water_level_m_navd88, v_lag5)) + 
  geom_point(alpha = 0.05) +
  geom_smooth(se = F) + 
  facet_wrap(~tide_limb, nrow = 1)




