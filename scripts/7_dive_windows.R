## This script looks at tidal stages in the context of slack tides to understand
## operational windows
##
## 2025-08-21
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")
p_load(plotly, # look closer at data
       zoo, # gapfilling
       VulnToolkit, # ID tides
       suncalc) # sunrise/sunset times

tide <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv")

adcp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250703_adcp_velocity_L1.csv")

df <- left_join(tide, adcp, by = "time_pst") %>% 
  assign_season()

p1 <- ggplot(df, aes(time_pst, water_level_m_navd88)) + geom_line()

ggplotly(p1)

## Set MCRL dock lat and long for sunrise/sunset calculations
latitude = 48.0793816189378
longitude = -123.04514333140335

x <- df %>% 
  filter(time_pst > "2023-01-15" & 
           time_pst < "2023-02-20") %>% 
  mutate(max_v_m_s_gf = na.approx(max_velocity_m_s, na.rm = FALSE)) %>% 
  drop_na(water_level_m_navd88, max_v_m_s_gf) %>% 
  #rowwise() %>%
  mutate(sunrise = getSunlightTimes(date = date, lat = latitude, lon = longitude, tz = "Etc/GMT+8")$sunrise,
    sunset = getSunlightTimes(date = date, lat = latitude, lon = longitude, tz = "Etc/GMT+8")$sunset) %>% 
  mutate(sunrise = force_tz(sunrise, tzone = "UTC"), 
         sunset = force_tz(sunset, tzone = "UTC")) %>% 
  mutate(day_or_night = case_when(
      time_pst >= sunrise & time_pst <= sunset ~ "day",  # Between sunrise and sunset
      TRUE ~ "night"))                                   # Outside sunrise and sunset

plot_grid(ggplot(x, aes(time_pst, water_level_m_navd88)) + geom_line(), 
          ggplot(x, aes(time_pst, max_v_m_s_gf)) + geom_line(), 
          ncol = 1)


## Let's pick a time-frame with good datasets and calculate a bunch of stuff.

tide1_start <- as.POSIXct("2023-01-19", tz = "UTC")
tide1_end <- as.POSIXct("2023-01-25", tz = "UTC")

p2 <- ggplot(x, aes(time_pst, water_level_m_navd88, color = max_v_m_s_gf)) + 
  geom_line() + 
  scale_color_viridis_c(option = "turbo") + 
  annotate(geom = "rect", xmin = tide1_start, 
                              xmax = tide1_end, 
                              ymin = -1.3, ymax = 2.6, 
           alpha = 0.2, color = "gray") + 
  labs(x = "", y = "Water level \n (m NAVD88)", color = "Velocity \n (max, m/s)")

p2


## First, isolate your time-period
y0 <- x %>% 
  filter(time_pst > tide1_start & 
           time_pst < tide1_end) %>% 
  dplyr::select(-contains("tide"))

## Second, assign tides
y <- y0  %>% 
  left_join(., VulnToolkit::HL(level = y0$water_level_m_navd88, 
                            time = y0$time_pst) %>% 
              rename("time_pst" = time) %>% 
              dplyr::select(-level), 
            by = "time_pst") %>% 
  mutate(tide3 = ifelse(tide == "H", "H", NA)) %>% 
  group_by(date) %>% 
  mutate(interhighs = case_when(tide3 == "H" ~ 1, 
                               TRUE ~ 0)) %>% 
  mutate(interhighs = cumsum(interhighs)) %>% 
  mutate(interhighs = ifelse(interhighs == 1, 
                             ifelse(row_number() %in% head(which(interhighs == 1), 8) |
                                      row_number() %in% tail(which(interhighs == 1), 8), 
                                    0, interhighs), 
                             interhighs)) %>% 
  mutate(low_tide = case_when(
    interhighs == 1 & 
      water_level_m_navd88 >= min(water_level_m_navd88[interhighs == 1]) & 
      water_level_m_navd88 <= min(water_level_m_navd88[interhighs == 1]) + 0.03048 ~ "L",
    TRUE ~ NA_character_)) %>% 
  mutate(vel_cat = case_when(max_v_m_s_gf < 0.5 ~ "0-0.5", 
                             max_v_m_s_gf > 0.5 & max_v_m_s_gf < 1 ~ "0.5-1", 
                             max_v_m_s_gf > 1 ~ "1+"))

ggplot(y, aes(time_pst, water_level_m_navd88)) + 
  geom_line() + 
  geom_point(data = y %>% filter(interhighs == 1), color = "gray") + 
  geom_point(data = y %>% filter(low_tide == "L"))

# Identify the ranges of "day" periods
day_ranges <- y %>%
  filter(day_or_night == "day") %>%
  group_by(date = as.Date(time_pst)) %>%   # Group by date to ensure proper range calculation
  summarize(
    xmin = min(time_pst),  # Start of day period
    xmax = max(time_pst)   # End of day period
  ) %>%
  ungroup()

dive_windows <- y %>% 
  filter(low_tide == "L") %>% 
  group_by(date) %>% 
  summarize(xmin = min(time_pst), 
            xmax = max(time_pst)) %>% 
  mutate(window = as.numeric(xmax - xmin))

p3 <- ggplot(y, aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "gray90", lwd = 1.2) +
  geom_line(aes(color = max_v_m_s_gf), lwd = 1) +
  geom_rect(data = day_ranges,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,    # Avoid inheriting aesthetics from the main plot
    fill = "yellow", alpha = 0.15) +  # Night shading with transparency
  geom_rect(data = dive_windows,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,    # Avoid inheriting aesthetics from the main plot
    fill = "blue", alpha = 0.15) +  # Night shading with transparency
  annotate(geom = "text", 
           x = dive_windows$xmin, 
           y = 2.5, 
           label = paste(dive_windows$window, " mins")) + 
  scale_color_viridis_c(option = "turbo") + 
  labs(x = "", y = "Water level \n (m NAVD88)", color = "Velocity \n (max, m/s)")

plot_grid(p2, p3, ncol = 1)
ggsave("250821_")

# 1. find low tide between two high tides
# 2. check that velocities are low
# 3. check it's not night
## 2 and 3 in a case_when, what about 1?
## Check out VulnToolkit



