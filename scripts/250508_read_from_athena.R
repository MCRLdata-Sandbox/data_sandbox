## Prep data pulled via Athena for tidegauge, ctd, ph, met, and cdom for 
## calculating natural variability envelopes
##
## 2025-05-08
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, 
       cowplot,
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






