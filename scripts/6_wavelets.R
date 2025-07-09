## This script explores the temporal structure of variables using wavelet anlaysis, 
##and the relationships between the temporal structure of variables using wavelet 
## coherence. Starting point: water level and velocities
##
## 2025-07-09
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")

p_load(WaveletComp, 
       tictoc, 
       zoo)

adcp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250703_adcp_velocity_L1.csv") %>% 
  assign_season()

tidegauge <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv")

df <- left_join(adcp, tidegauge, by = "time_pst")

x <- df %>% 
  drop_na(max_velocity_m_s, water_level_m_navd88) %>% 
  filter(time_pst > "2021-10-01" &
           time_pst < "2022-02-13") %>%
  # rename("wlm" = water_level_m_navd88, 
  #        "velms" = max_velocity_m_s,
  #        "timepst" = time_pst) %>% 
  dplyr::select(time_pst, water_level_m_navd88, max_velocity_m_s) %>% 
  as.data.frame() ## Won't run without it, will run with it.... stupid.

plot_grid(ggplot(x, aes(time_pst, water_level_m_navd88)) + geom_line(), 
          ggplot(x, aes(time_pst, max_velocity_m_s)) + geom_line(), 
          ncol = 1)

tic("Run WT")
wt <- analyze.wavelet((my.data = x), my.series = "max_velocity_m_s")
toc()

tic("run WC")
wc <- analyze.coherency(my.data = x, my.pair = c("max_velocity_m_s", "max_velocity_m_s"))
toc()

wc.image(wc)




