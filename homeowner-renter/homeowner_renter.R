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

no_missing <- data %>%
  # keep just homeowners and renters respondents
  filter(PERNUM==1 & !(is.na(OWNERSHP)) & OWNERSHP!=21) %>%
  group_by(YEAR, OWNERSHP) %>%
  # total count, counts by gender and race
  summarise(count = n(), n_male=sum(SEX==1), n_white=sum(RACE==100)) %>%
  mutate(yr_total = sum(count), prop=100*count/yr_total, male_prop=100*n_male/count, white_prop=100*n_white/count)

no_missing$OWNERSHP <- as.character(no_missing$OWNERSHP)
no_missing$YEAR <- as.character(no_missing$YEAR)

## how has the proportion of homeowners and renters changed over time?
ggplot(no_missing) + geom_line(aes(x=YEAR, y=prop, group=OWNERSHP))

# gender ratio of homeowners and renters over time
g <- ggplot(no_missing, aes(x=YEAR, y=male_prop, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
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

# race ratio of homeowners and renters over time
r <- ggplot(no_missing, aes(x=YEAR, y=white_prop, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(male_prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=5) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  labs(title="White Ratio of Homeowners and Renters", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6F59", "#61D095")) + 
  scale_y_continuous(limits=c(0,100))


# put together several of the demographic trends charts
ggarrange(g, r,
          ncol = 1, nrow = 2)







