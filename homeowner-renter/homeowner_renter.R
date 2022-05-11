## homeowner-renter.R
## Visualize IPUMS CPS ASEC data on homeowners and renters in the U.S. over time
## Requires: cps_00013.xml
## Last edited by Tal Roded, 5/11/22
######################################
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ipumsr)

# set working directory
setwd("C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/visualize-curiosity/homeowner-renter")

# read in data
ddi <- read_ipums_ddi("data/cps_00013.xml")
data <- read_ipums_micro(ddi)
