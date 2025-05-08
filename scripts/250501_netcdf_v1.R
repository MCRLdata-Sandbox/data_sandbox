## This script takes in test netCDF files downloaded manually from AWS S3 to learn
## about data structure, develop code for reading and formatting, and hopefully 
## lay the bedrock useful for directly pulling info from S3
## Steps for this script: 
### 1. Read in data manually
### 2. Set up a function and test it on a couple
### 3. Figure out how to generalize function to other datasets

# QC flags
# 0 = good
# 1 = missing values
# 2 = too low
# 4 = too high
# 8 = spike detection/non-real data

## Access Key ID: AKIATW2JYZRGMAILEAME
## Secrete Key: PwBtCRdEb/qtxAT9h/YlRGkTebAnBODbDtDKvaEdd

# 0. Setup ---------------------------------------------------------------------

require(pacman)

p_load(tidyverse, 
       lubridate,
       parsedate,
       ncdf4)

theme_set(theme_bw())


# 1. Manually read in a netCDF file --------------------------------------------

## Can tell from the name this is tide gauge from 8/1/2023

## Reads it in, not a normal file structure I'm used to looking at...
## Pretty opaque as an R object
tide_nc <- nc_open("data/mcrl_data/netcdf/test/tide_gauge.mcrl_pier-ysi_nile_1-5min.a1.20230801.nc")

## This is what chat told me to do for pulling which variables are in the file
variable_names <- sapply(tide_nc$var, function(x) x$name)

## We know the file is not in GMT, because that's baked in: 
tide_nc$is_GMT


ncatt_get(tide_nc, 0)

time_dim <- tide_nc$var$water_level$dim[[1]]

time_units <- time_dim$units

variable_names[1]

## 
ncvar_get(tide_nc, "water_level")

ncvar_get(tide_nc, "qc_water_level")

ncvar_get(tide_nc, "water_level_smooth")

df <- tibble(time_raw = ncvar_get(tide_nc, "time"), 
             wl_m = ncvar_get(tide_nc, "water_level"), 
             wl_q = ncvar_get(tide_nc, "qc_water_level"), 
             wl_smooth = ncvar_get(tide_nc, "water_level_smooth")) %>% 
  mutate(index = 1:n())

ggplot(df, aes(wl_m, wl_smooth)) + 
  geom_point()


df %>% 
  mutate(datetime = parsedate::parse_date(time_raw))



### We are now going to try and match Athena's CSV data to netCDF to reverse-engineer
### how to get information like time-zone right

rm(list = ls())

## First, read in one netCDF

nc1 <- nc_open("data/mcrl_data/netcdf/test/tide_gauge.mcrl_pier-ysi_nile_1-5min.a1.20250102.nc")

ncdf4::ncatt_get(nc1)

time <- ncvar_get(nc1, "time")
tunits <- ncatt_get(nc1,"time","units")

df <- tibble(time = ncvar_get(nc1, "time"), 
             wl_m = ncvar_get(nc1, "water_level"), 
             wl_q = ncvar_get(nc1, "qc_water_level")) %>% 
  mutate(index = 1:n()) %>% 
  mutate(datetime = round_date(as_datetime(time)), unit = "5 min")









