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

air_temp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_air_temp_L1.csv")

ctd <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv")



# 2. DOY summary of min, mean, and max -----------------------------------------

doy_summaries <- full_join(air_temp, 
          ctd, 
          by = "time_pst") %>% 
  mutate(doy = yday(time_pst)) %>% 
  group_by(doy) %>% 
  summarize(across(all_of(c("airtemp_avg_deg_c", "temp_deg_c", "salinity_psu_clean")), 
                   list(max = max, mean = mean, min = min), na.rm = TRUE)) %>% 
  complete(doy = 1:366) 

## This is a bit of a cheating work-around: some air temp data is being lost at
## the expense of ctd data... but it makes the plots interpretable
plot_doy <- function(min, mean, max, ylab){
  ggplot(doy_summaries %>% drop_na(), aes(doy)) + 
    geom_ribbon(aes(ymin = ifelse(is.na({{min}}), NA, {{min}}), 
                    ymax = ifelse(is.na({{max}}), NA, {{max}})), 
                alpha = 0.4, fill = "red", na.rm = T) + 
    geom_smooth(aes(y = {{mean}}), lwd = 1, color = "black") + 
    labs(x = "Day of Year", y = ylab)
}

air_plot <- plot_doy(airtemp_avg_deg_c_min, 
         airtemp_avg_deg_c_mean, 
         airtemp_avg_deg_c_max, 
         "Air Temperature (C)") + 
  geom_hline(yintercept = 10, linetype = "dashed")

water_plot <- plot_doy(temp_deg_c_min, 
                     temp_deg_c_mean, 
                     temp_deg_c_max, 
                     "Water Temperature (C)") + 
  geom_hline(yintercept = 10, linetype = "dashed")

temp_diff <- ggplot(doy_summaries %>% drop_na(), 
                    aes(doy, airtemp_avg_deg_c_mean - temp_deg_c_mean)) + 
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = 0, ymax = Inf,
           fill = "blue", alpha = 0.1) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -Inf, ymax = 0,
           fill = "red", alpha = 0.1) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  annotate("text", 
           x = 50, y = 2.5, 
           label = "Air is warmer",
           color = "blue",
           size = 4) +
  annotate("text", 
           x = 180, y = -3, 
           label = "Water is warmer", 
           color = "red",
           size = 4) + 
  labs(x = "Day of Year", 
       y = "Air temp - Water temp (C)")

plot_grid(air_plot, water_plot, temp_diff, 
          rel_widths = c(1, 1, 1.2),
          nrow = 1)
ggsave("figures/250630_temps_by_doy.png", width = 10, height = 3)


sal_plot <- plot_doy(salinity_psu_clean_min, 
                     salinity_psu_clean_mean, 
                     salinity_psu_clean_max, 
                     "Salinity (PPT)")

sal_v_temp <- ggplot(doy_summaries, aes(airtemp_avg_deg_c_mean, salinity_psu_clean_mean, color = doy)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_point() + 
  scale_color_viridis_c() + 
  labs(x = "Mean Air Temp (C)", 
       y = "Mean Salinity (PPT)", 
       color = "Day of \n Year")

plot_grid(sal_plot, sal_v_temp, 
          rel_widths = c(1, 1.5),
          nrow = 1)
ggsave("figures/250630_salinity.png", width = 7, height = 3)




