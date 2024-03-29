## homeowner-renter.R
## Visualize IPUMS CPS ASEC data on homeowners and renters in the U.S. over time
## Requires: cps_00013.xml
## Last edited by Tal Roded, 10/11/22
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

cleaned_data <- data %>%
  # keep just homeowners and renters respondents
  filter(PERNUM==1 & !(is.na(OWNERSHP)) & OWNERSHP!=21)

###########################################
## Dimensions of ownership status over time
###########################################
## How does homeownership compare by gender?
genders <- cleaned_data %>%
  group_by(YEAR, OWNERSHP) %>%
  # total count, counts by gender and race
  summarise(count = n(), n_male=sum(SEX==1)) %>%
  mutate(yr_total = sum(count), prop=100*count/yr_total, male_prop=100*n_male/count)

genders$OWNERSHP <- as.character(genders$OWNERSHP)
genders$YEAR <- as.character(genders$YEAR)
g <- ggplot(genders, aes(x=YEAR, y=male_prop, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(male_prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=8) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=24), 
                                  axis.text.y = element_blank()) + 
  labs(title="Male Ratio of Homeowners and Renters", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=28)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 32, hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6F59", "#61D095"), 
                    labels=c("Homeowners", "Renters")) + 
  scale_y_continuous(limits=c(0,85), breaks=seq(0,85,20))
g
ggsave(plot=g, "charts/gender_ratio.png", width=16, height=12)


## How does ownership compare by race?
races <- cleaned_data %>%
  group_by(YEAR, OWNERSHP, RACE) %>%
  summarise(count=n()) %>%
  group_by(YEAR, RACE) %>%
  mutate(yr_owner_total = sum(count), prop=100*count/yr_owner_total) %>%
  # select several groups of interest - white, black, asian
  filter(RACE==100 | RACE==200 | RACE==651)

races$OWNERSHP <- as.character(races$OWNERSHP)
races$YEAR <- as.numeric(races$YEAR)
races$RACE <- as.character(races$RACE)

races_home <- races %>%
  filter(OWNERSHP==10) %>%
  filter(YEAR!=2019 & YEAR!=2020)
r <- ggplot(races_home, aes(x=YEAR, y=prop, fill=RACE)) + 
  geom_bar(position="dodge", stat="identity", width=6) + 
  geom_text(aes(label = paste0(round(prop), "%")), color="black", vjust=1.5, position=position_dodge(6), size=7) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=24), 
                                  axis.text.y = element_blank()) + 
  labs(title="Race Ratios of Homeowners", y=NA) +
  theme(legend.position="bottom", legend.title=element_blank(), legend.text=element_text(size=28)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 32, hjust = 0.5)) +
  scale_fill_manual(values=c("#CC4BC2", "#FC7A57", "#FCD757"), 
                    labels=c("White", "Black", "Asian"))
r

races_rent <- races %>%
  filter(OWNERSHP==22) %>%
  filter(YEAR!=2019 & YEAR!=2020)
r2 <- ggplot(races_rent, aes(x=YEAR, y=prop, fill=RACE)) + 
  geom_bar(position="dodge", stat="identity", width=6) + 
  geom_text(aes(label = paste0(round(prop), "%")), color="black", vjust=1.5, position=position_dodge(6), size=7) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=24), 
                                  axis.text.y = element_blank()) + 
  labs(title="Race Ratios of Renters", y=NA) +
  theme(legend.position="none") +
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 32, hjust = 0.5)) +
  scale_fill_manual(values=c("#CC4BC2", "#FC7A57", "#FCD757"))
r2
# put together several of the demographic trends charts
r_both <- ggarrange(r, r2,
          ncol = 1, nrow = 2)
r_both
ggsave(plot=r_both, "charts/race_ratios.png", width=18, height=16)


## How does homeownership compare by college degree attainment?
degree <- cleaned_data %>%
  group_by(YEAR, OWNERSHP) %>%
  # drop missing education observation
  filter(EDUC<999) %>%
  # total count, counts by college degree
  summarise(count = n(), n_college=sum(EDUC>=111)) %>%
  # calculate proportion with at least a college degree
  mutate(yr_total = sum(count), prop=100*count/yr_total, college_prop=100*n_college/count)

degree$OWNERSHP <- as.character(degree$OWNERSHP)
degree$YEAR <- as.character(degree$YEAR)

# college degree ratio of homeowners and renters over time
g <- ggplot(degree, aes(x=YEAR, y=college_prop, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(college_prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=8) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=24), 
                                  axis.text.y = element_blank()) + 
  labs(title="Ratio of Homeowners and Renters w/ At Least A Bachelor's Degree", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=28)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 32, hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6F59", "#61D095"), 
                    labels=c("Homeowners", "Renters")) + 
  scale_y_continuous(limits=c(0,50), breaks=seq(0,50,10))
g
ggsave(plot=g, "charts/college_ratio.png", width=16, height=12)

## What are the average ages of homeowners and renters over time?
age <- cleaned_data %>%
  group_by(YEAR, OWNERSHP) %>%
  # average age by year and ownership status
  summarise(mean_age = mean(AGE))

age$OWNERSHP <- as.character(age$OWNERSHP)
age$YEAR <- as.character(age$YEAR)

# mean age of homeowners and renters over time
g <- ggplot(age, aes(x=YEAR, y=mean_age, group=OWNERSHP, fill=OWNERSHP)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = round(mean_age)), color="black", vjust=1.5, position=position_dodge(0.9), size=8) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=24), 
                                  axis.text.y = element_blank()) + 
  labs(title="Mean Age of Homeowners and Renters", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=28)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 32, hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6F59", "#61D095"), 
                    labels=c("Homeowners", "Renters")) + 
  scale_y_continuous(limits=c(0,60), breaks=seq(0,60,15))
g
ggsave(plot=g, "charts/mean_age.png", width=16, height=12)


## how do income changes compare by ownership
incomes <- cleaned_data %>%
  group_by(YEAR, OWNERSHP) %>%
  summarise(count=n(), mean_inc=mean(INCTOT)) %>%
  filter(YEAR!=2019 & YEAR!=2020) %>%
  ungroup %>%
  group_by(OWNERSHP) %>%
  mutate(inc_growth=100*mean_inc/mean_inc[1])
incomes$OWNERSHP <- as.character(incomes$OWNERSHP)
incomes$YEAR <- as.character(incomes$YEAR)

ggplot(incomes, aes(x=YEAR, y=inc_growth, group=OWNERSHP, fill=OWNERSHP)) + 
  geom_bar(position="dodge", stat="identity", width=0.5)

###########################################
## Effects of ownership trends
###########################################
## What proportion of homeowners are also on food stamps?

