## homeowner-renter.R
## Visualize IPUMS CPS ASEC data on homeowners and renters in the U.S. over time
## Requires: cps_00013.xml
## Last edited by Tal Roded, 5/12/22
######################################
library(tidyverse)
library(ggthemes) # for fivethirtyeight theme
library(RColorBrewer) # custom colors
library(ipumsr) # for easily reading in IPUMS data
library(ggpubr) # for placing multiple charts on the same output


# set working directory
setwd("C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/visualize-curiosity/homeowner-renter")

# read in data
ddi <- read_ipums_ddi("data/cps_00013.xml")
data <- read_ipums_micro(ddi)

genders <- data %>%
  # keep just homeowners and renters respondents
  filter(PERNUM==1 & !(is.na(OWNERSHP)) & OWNERSHP!=21) %>%
  group_by(YEAR, OWNERSHP) %>%
  # total count, counts by gender and race
  summarise(count = n(), n_male=sum(SEX==1)) %>%
  mutate(yr_total = sum(count), prop=100*count/yr_total, male_prop=100*n_male/count)

genders$OWNERSHP <- as.character(genders$OWNERSHP)
genders$YEAR <- as.character(genders$YEAR)

races <- data %>%
  # keep just homeowners and renters respondents
  filter(PERNUM==1 & !(is.na(OWNERSHP)) & OWNERSHP!=21) %>%
  group_by(YEAR, OWNERSHP, RACE) %>%
  summarise(count=n()) %>%
  group_by(YEAR, RACE) %>%
  mutate(yr_owner_total = sum(count), prop=100*count/yr_owner_total) %>%
  # select several groups of interest - white, black, asian
  filter(RACE==100 | RACE==200 | RACE==651)

races$OWNERSHP <- as.character(races$OWNERSHP)
races$YEAR <- as.numeric(races$YEAR)
races$RACE <- as.character(races$RACE)


## how has the proportion of homeowners and renters changed over time?
ggplot(no_missing) + geom_line(aes(x=YEAR, y=prop, group=OWNERSHP))

# gender ratio of homeowners and renters over time
g <- ggplot(genders, aes(x=YEAR, y=male_prop, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(male_prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=5) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  labs(title="Gender Ratio of Homeowners and Renters", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6F59", "#61D095")) + 
  scale_y_continuous(limits=c(0,100))
g

# race ratio of homeowners and renters over time
r <- ggplot(races, aes(x=YEAR, y=prop, color=RACE, linetype=OWNERSHP)) + geom_line() +  
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  labs(title="Race Ratios of Homeowners and Renters", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_y_continuous(limits=c(0,100))
r

# put together several of the demographic trends charts
ggarrange(g, r,
          ncol = 1, nrow = 2)







