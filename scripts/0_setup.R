## This script provides general setup for all other scripts, including loading
## a set of general-use packages, establishing functions used in multiple scripts
## and other general formatting things

## Clean your environment
rm(list = ls())

## Load packages
## If you've never installed the pacman package, uncomment the line below and run once
## install.packages("pacman")
require(pacman)
p_load(tidyverse, # keep your data workflows tidy
       cowplot, # arrange multiple plots
       janitor,  # clean_names()
       hms) # as_hms()

## Set ggplot theme
theme_set(theme_bw())


