## homeowner-renter.R
## Visualize trends in homeowners and renters in the US over time
## Requires: cps_00013.xml
## Last edited by Tal Roded, 5/9/22
######################################
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ipumsr)

# set working directory
setwd("C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/visualize-curiosity/homeowner-renter")

# read in data
ddi <- read_ipums_ddi("cps_00013.xml")
data <- read_ipums_micro(ddi)

