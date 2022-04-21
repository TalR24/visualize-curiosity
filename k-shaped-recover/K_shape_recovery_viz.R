## K_shape_recovery_viz.R
## Visualize IPUMS ATUS data comparing time use habits by income tiers for
## 2019 and 2020.
## Requires: atus_data_for_charting.xlsx
## Last edited 4/20/22 by Tal Roded
############################################################################
library(tidyverse)
library(readxl)
library(ggthemes)
library(directlabels)
library(RColorBrewer)

## working directory
setwd("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/K_Shaped_Recovery")

#####################################
## Compare distributions of rich and poor - education, occupations
#####################################
## Education bar chart
edu <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "educ_income_tiers")
edu$educ <- factor(edu$educ, levels=c("1", "2", "3", "4"), 
                    labels=c("High School or Less", "Some college", 
                                        "Bachelor's degree", "Advanced degree"))
edu$income_tiers <- factor(edu$income_tiers, levels=c("low", "high"), 
                                labels=c("Low-Income", "High-Income"))

p <- ggplot(edu, aes(x=educ, y=prop, fill=income_tiers)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=14)) + 
  theme(axis.text.y = element_blank()) + 
  labs(title="Educational Attainment of the Rich and Poor", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/educations.png", plot = p, 
            width = 26, height = 16, units = "cm")

## Occupations bar chart
occ <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "occs_income_tiers")
occ$income_tiers <- factor(occ$income_tiers, levels=c("low", "high"), 
                                labels=c("Low-Income", "High-Income"))

p <- ggplot(occ, aes(x=occ2s, y=prop, fill=income_tiers)) + 
  geom_col(position=position_dodge(.5)) + 
  coord_flip() + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  labs(title="Occupations of the Rich and Poor") + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.35)) +
  scale_y_continuous(labels=c("0%", "10%", "20%", "30%"), expand=c(0,0)) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/occupations.png", plot = p, 
            width = 24, height = 16, units = "cm")


#####################################
## Compare distributions of rich and poor by year
#####################################
## Education bar chart
act2019 <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "activities_2019")
act2019$income_tiers <- factor(act2019$income_tiers, levels=c("low", "high"), 
                                labels=c("Low-Income", "High-Income"))

p <- ggplot(act2019, aes(x=activity, y=avg_dur_hour, fill=income_tiers)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = round(avg_dur_hour, digits=2)), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  theme(axis.text.y = element_blank()) + 
  labs(title="Average Time Spent on Selected Activities by the Rich and Poor, 2019", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/activities2019.png", plot = p, 
            width = 26, height = 16, units = "cm")


#####################################
## Compare activity time durations by income tiers and years
#####################################
## Average time spent
actcomps <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "act_comps_tiers_years")
actcomps$year <- as.character(actcomps$year)
actcomps$activity <- factor(actcomps$activity, levels=c("pcare", "work", "social", "hhact", "food", 
                                                        "travel", "purch", "sports"), 
                   labels=c("Personal Care", "Working", "Socializing/Leisure", "Homecare", "Eating/drinking", 
                            "Traveling", "Shopping", "Sports/Recreation/Exercise"))
actcomps$income_tiers <- factor(actcomps$income_tiers, levels=c("low", "high"), 
                            labels=c("Low-Income", "High-Income"))

p <- ggplot(actcomps, aes(x=year, y=duration, fill=income_tiers)) + geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~activity, scales='free') + 
  geom_text(aes(label = round(duration, digits=1)), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=14)) + 
  theme(axis.text.y = element_blank()) + 
  labs(title="Average Hours Spent on Selected Activities by the Rich and Poor \n", y=NA) + 
  theme(legend.position=c(0.82,0.2), legend.title=element_blank(), legend.text=element_text(size=20), 
        legend.direction = "vertical", legend.spacing.y = unit(1, "cm")) + 
  guides(fill=guide_legend(byrow=T)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(strip.text.x = element_text(size=15, face="bold")) + 
  theme(panel.spacing.x  = unit(1.5, "lines")) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/activities_tiers_years.png", plot = p, 
            width = 24, height = 24, units = "cm")


## Average time spent by those who did the activity
actcompsN <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "act_comps_tiers_yearsN")
actcompsN$year <- as.character(actcompsN$year)
actcompsN$activity <- factor(actcompsN$activity, levels=c("pcare", "work", "social", "hhact", "food", 
                                                        "travel", "purch", "sports"), 
                            labels=c("Personal Care", "Working", "Socializing/Leisure", "Homecare", "Eating/drinking", 
                                     "Traveling", "Shopping", "Sports/Recreation/Exercise"))
actcompsN$income_tiers <- factor(actcompsN$income_tiers, levels=c("low", "high"), 
                                labels=c("Low-Income", "High-Income"))

p <- ggplot(actcompsN, aes(x=year, y=duration, fill=income_tiers)) + geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~activity, scales='free') + 
  geom_text(aes(label = round(duration, digits=1)), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=14)) + 
  theme(axis.text.y = element_blank()) + 
  labs(title="Average Hours Spent on Selected Activities by the Rich and Poor", y=NA, 
       subtitle="Among those who participated in the activity \n") + 
  theme(legend.position=c(0.82,0.2), legend.title=element_blank(), legend.text=element_text(size=20), 
        legend.direction = "vertical", legend.spacing.y = unit(1, "cm")) + 
  guides(fill=guide_legend(byrow=T)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 18, face = "italic")) + 
  theme(strip.text.x = element_text(size=15, face="bold")) + 
  theme(panel.spacing.x  = unit(1.5, "lines")) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/activities_tiers_years_selected.png", plot = p, 
            width = 24, height = 24, units = "cm")


#####################################
## Compare activity time durations by locations
#####################################
## Average time spent
actloc <- read_excel("data/atus_data_for_charting.xlsx", col_names = TRUE, sheet = "act_locations")
actloc$year <- as.character(actloc$year)
actloc$income_tiers <- factor(actloc$income_tiers, levels=c("low", "high"), 
                                labels=c("Low-Income", "High-Income"))

p <- ggplot(actloc, aes(x=year, y=duration, fill=income_tiers)) + geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~location, scales='free') + 
  geom_text(aes(label = round(duration, digits=1)), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=14)) + 
  theme(axis.text.y = element_blank()) + 
  labs(title="Average Hours Spent at Selected Locations by the Rich and Poor", y=NA, 
       subtitle="Among those who spent any time at location \n") + 
  theme(legend.position=c(0.88,0.14), legend.title=element_blank(), legend.text=element_text(size=20), 
        legend.direction = "vertical", legend.spacing.y = unit(1, "cm")) + 
  guides(fill=guide_legend(byrow=T)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 18, face = "italic")) + 
  theme(strip.text.x = element_text(size=14, face="bold")) + 
  theme(panel.spacing.x  = unit(1.5, "lines")) + 
  scale_fill_manual(values=c("#FF6F59", "#30BCED"))
p <- ggsave("charts/activities_locations.png", plot = p, 
            width = 24, height = 28, units = "cm")
