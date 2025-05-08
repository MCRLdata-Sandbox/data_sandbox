## Prep data pulled via Athena for tidegauge, ctd, ph, met, and cdom for 
## calculating natural variability envelopes
##
## 2025-05-08
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

rm(list = ls())

require(pacman)
p_load(tidyverse, 
       cowplot,
       openair,
       grid,
       gridExtra,
       janitor,  # clean_names()
       hms) # as_hms()

theme_set(theme_bw())


# 2. Discover files ------------------------------------------------------------

## We have several files downloaded as csvs directly from Athena. In an effort to
## make things as plug-and-play as possible, I'm going to add extra steps to ID
## which files are which and read them in, hopefully programmatically
all_files <- list.files("data/mcrl_data/athena/210501_250501", 
                        full.names = T)

what_files_do_i_have <- function(file){
  headers <- read_csv(file) %>% 
    # we aren't interested in time or qc, just data cols
    dplyr::select(-c(contains("time"), contains("qc_"))) %>%  
    # make things easier to read, w/ no special characters to potential cause issues
    clean_names() 
  
  paste(colnames(headers), collapse = ", ")
}

file_vars <- all_files %>% 
  map(what_files_do_i_have)


# 3. Read in files -------------------------------------------------------------

## I suspect there's a cleaner way to do this, but it's likely most efficient to
## label files when you're downloading.

## Helper function to find the string you're looking for
which_file <- function(var_string){
  which(sapply(file_vars, grepl, pattern = var_string))[[1]]
}

## One more helper function to clean up column names, round datetime, and convert time to PST
read_csv_formatted <- function(file)(
  read_csv(file) %>% 
    clean_names() %>% 
    mutate(time_utc = round_date(force_tz(time_utc, tzone = "UTC"), unit = "5 min")) %>% 
    mutate(time_pst = with_tz(time_utc, tzone = "Etc/GMT+8")) %>%  # Forcing to PST
    mutate(year = year(time_pst), 
           quarter = quarter(time_pst, fiscal_start = 3),
           month = month(time_pst), 
           doy = yday(time_pst), 
           hour_of_day = hour(time_pst))
)

## Let's read in our 5 datasets
tidegauge <- read_csv_formatted(all_files[[which_file("water_level")]])
ctd <- read_csv_formatted(all_files[[which_file("salinity")]])
ph <- read_csv_formatted(all_files[[which_file("ph")]]) ## this is bad regex, need to isolate ph
cdom <- read_csv_formatted(all_files[[which_file("cdom")]]) 
met <- read_csv_formatted(all_files[[which_file("airtemp")]])


## Try this again...
create_wind_rose <- function(data, selected_quarter, title){
  data %>% 
    filter(quarter == selected_quarter) %>% 
    windRose(., ws = "windspeed_avg_m_s", wd = "winddir_deg_from", key.position = "bottom", main = title)
}

p0 <- windRose(met, ws = "windspeed_avg_m_s", wd = "winddir_deg_from", key.position = "bottom", main = "Annual")
p1 <- create_wind_rose(met, 1, "Mar-May")
p2 <- create_wind_rose(met, 2, "Jun-Aug")
p3 <- create_wind_rose(met, 3, "Sep-Nov")
p4 <- create_wind_rose(met, 4, "Dec-Feb")

save_windrose_png <- function(plot_name, name){
  png(filename = paste0("figures/wind_plots/", name, ".png"))
  plot(plot_name)
  dev.off()
}

save_windrose_png(p0, "annual")
save_windrose_png(p1, "spring")
save_windrose_png(p2, "summer")
save_windrose_png(p3, "fall")
save_windrose_png(p4, "winter")


grid.arrange(p1)

# Function to convert plot to a grob using grid package
plot_to_grob <- function(plot_function) {
  plot.new()    # Start a new plot to ensure it draws correctly
  capture.output(plot_function)  # Execute the plot function for visualization
  recorded_plot <- recordPlot()  # Capture the plot
  return(recorded_plot)
}

p1 <- plot_to_grob(create_wind_rose(met, 1, "Jan-Mar"))
p2 <- plot_to_grob(create_wind_rose(met, 2, "Apr-Jun"))

list(p1, p2)

grid.arrange(grobs = p1, ncol = 2)

plot_grid(plot_to_grob(create_wind_rose(met, 1, "Jan-Mar")), 
          plot_to_grob(create_wind_rose(met, 2, "Apr-Jun")), 
          ncol = 1)



x <- met %>% 
  filter(quarter == 4) %>% 
  windRose(ws = "windspeed_avg_m_s", wd = "winddir_deg_from", key.position = "bottom", main = "")



# Create bins for wind speed and direction
met %>%
  drop_na(windspeed_avg_m_s, winddir_deg_from) %>% 
  mutate(wind_speed_bin = cut(windspeed_avg_m_s, 
                         breaks = c(0, 5, 10, 15),
                         include.lowest = TRUE,
                         labels = c("0-5", "5-10", "10-15")),
    wind_dir_bin = cut(winddir_deg_from,
                       breaks = seq(0, 360, by = 30),
                       include.lowest = TRUE,
                       labels = paste(seq(0, 330, by = 30), seq(30, 360, by = 30), sep = "-"))) %>% 
  ggplot(aes(x = wind_dir_bin, fill = wind_speed_bin)) +
  geom_bar(width = 1) +
  coord_polar() +
  theme_minimal() +
  labs(title = "Wind Rose",
    x = "Wind Direction (degrees)",
    fill = "Wind Speed (m/s)") +
  theme(axis.title.x = element_blank())

ggplot(met, aes(time_pst, winddir_deg_from)) + geom_line()

# 4. Initial QC - coming soon! -------------------------------------------------


# 5. Initial plots -------------------------------------------------------------

make_plots <- function()

tidegauge_clean <- tidegauge %>% 
  mutate(date = as_date(time_pst)) %>% 
  filter(qc_water_level == 0) %>% 
  filter(time_pst >= "2023-06-01") %>% 
  filter(time_pst <= "2024-06-01") ## Taking a shortcut, picking a year that looks good

p1 <- tidegauge_clean %>% 
  ggplot(aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "blue", alpha = 0.5) + 
  labs(x = "", y = "WL (m NAVD88)")

p2 <- tidegauge_clean %>% 
  ggplot(aes(as.factor(month), water_level_m_navd88)) + 
  geom_boxplot()

p3 <- tidegauge_clean %>% 
  mutate(quarter = quarter(time_pst)) %>% 
  group_by(quarter, date, hour_of_day) %>% 
  summarize(water_level_m_navd88 = mean(water_level_m_navd88, na.rm = T)) %>% 
  ggplot(aes(hour_of_day, water_level_m_navd88)) + 
  geom_path(aes(group = date), color = "gray") + 
  geom_smooth(se = F) + 
  facet_wrap(~quarter) + 
  labs(x = "Hour of day", y = "WL (m NAVD88)")

plot_grid(p1, p3, 
          ncol = 1, 
          rel_heights = c(0.5, 1))

ggsave("figures/250508_tidegauge_natural_variability_v1.png", width = 7, height = 8)



## Make a plot for salinity, temp (air and water), ph, do (lower priority), 
## make those as tables linked in the readme
## wind speed/direction, esp if summarized by season - radar chart


## quick reference table for plot - what is the avg salinity of Sequim Bay?
ctd %>% 
  filter(qc_salinity == 0) %>% 
  mutate(month = month(time_pst)) %>% 
  ggplot(aes())
  
  ggplot(aes(as.factor(month), salinity_ppt)) + 
  geom_boxplot()

ctd %>% 
  filter(qc_temp == 0) %>% 
  mutate(month = month(time_pst)) %>% 
  ggplot(aes(as.factor(month), temp_deg_c)) + 
  geom_boxplot()




  group_by(month) %>% 
  summarize(mean_sal = mean(salinity_ppt, na.rm = T))








