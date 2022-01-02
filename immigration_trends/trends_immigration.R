## trends_immigration.R
## Code for "Trends in Immigration" Visualize Curiosity post
## Last edited by Tal Roded, 12/28/21
######################################
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ipumsr)

setwd("C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/trends_immigration")

## 2006-2018 ACS data pulled from IPUMS
ddi <- read_ipums_ddi("data/usa_00010.xml")
data <- read_ipums_micro(ddi)

## Time series of immigrants by origin country
immigrants_origin <- data %>%
  # dataset is huge so filtering this to the relevant sample first to speed things up
  filter(YRIMMIG > 0) %>%
  select(BPL, YEAR, HHWT) %>%
  group_by(BPL, YEAR) %>%
  summarise(count = n()) %>%
  group_by(BPL) %>%
  # keep just the top 10 groups by total immigration in the period, so the chart isn't too noisy
  mutate(total_immigrants = sum(count)) %>%
  ungroup() %>%
  mutate(rank = rank(-total_immigrants)) %>%
  group_by(BPL) %>%
  arrange(rank) %>% 
  slice_max(10)

%>%
  filter(rank<10)

  
  
  