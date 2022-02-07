## immigrant_trends_visualizations.R
## Visualize ACS data comparing US natives and immigrants for 2006-2019.
## Requires: ipums_data_for_mapping.xlsx
## Last edited 2/7/22 by Tal Roded
############################################################################
library(tidyverse)
library(readxl)
library(ggthemes)
library(directlabels)
library(RColorBrewer)
library(usmap)

## working directory
setwd("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/immigration_trends")

#####################################
## Time series of immigrants grouped by their birthplace
#####################################
bpl <- read_excel("data/ipums_data_for_mapping.xlsx", col_names = TRUE, sheet = "by_bpl")
bpl$bpl <- factor(bpl$bpl, levels=c("Other USSR/Russia", "Canada", "Philippines", "West Indies", 
                                       "AFRICA", "SOUTH AMERICA", "China", "India", "Central America", 
                                       "Mexico"),
                     labels=c("Russia", "Canada", "Philippines", "West Indies", 
                              "Africa", "South America", "China", "India", "Central America", 
                              "Mexico"))

p <- ggplot(bpl) + geom_line(aes(x=yrimmig, y=prop, group=bpl, color=bpl), size=1.3) + 
  theme_fivethirtyeight() + 
  theme(axis.text = element_text(size=14)) + 
  labs(title="Birthplaces of Immigrants, 2006-2019", subtitle="Among the top 10 birthplaces in 2019",
       y=NA) + expand_limits(x="2020") + 
  theme(legend.position="none") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) +
  scale_x_discrete(breaks=seq(2006, 2019, 2)) +
  scale_y_continuous(breaks=seq(0,25,5), labels=function(x) paste0(x, "%")) + 
  scale_color_manual(values=c("#363732", "#74226C", "#226F54", "#68534D", 
                              "#5A2328", "#2892D7", "#F06449", "#8FC93A", 
                              "#4B2142", "#537D8D"))
pd <- direct.label(p, "last.bumpup")
ggsave("charts/by_bpl.png", plot = pd, 
       width = 32, height = 16, units = "cm")

#####################################
## Compare distributions of natives and immigrants - edu, income
#####################################
## Education bar chart
edu <- read_excel("data/ipums_data_for_mapping.xlsx", col_names = TRUE, sheet = "educ_dists")
edu$educd <- factor(edu$educd, levels=c("No schooling", "Less than high school", "High School",
                                    "Some college", "Bachelor's degree", "Graduate degree"))

p <- ggplot(edu, aes(x=educd, y=prop, fill=native)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=13)) + 
  labs(title="Educational Attainment of 25+ year old Immigrants & U.S.-born", subtitle="Among immigrants who arrived to the U.S. between 2006-2019, natives in 2006-2019", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=14)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  scale_fill_manual(values=c("#FF6F59", "#61D095"))
p <- ggsave("charts/educations.png", plot = p, 
       width = 26, height = 16, units = "cm")

## Income distributions
inc <- read_excel("data/ipums_data_for_mapping.xlsx", col_names = TRUE, sheet = "educ_dists")

p <- ggplot(edu, aes(x=educd, y=prop, fill=native)) + geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = paste0(round(prop), "%")), color="black", vjust=1.5, position=position_dodge(0.9), size=6) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size=14)) + 
  labs(title="Educational Attainment of Immigrants & U.S.-born", subtitle="Among immigrants who arrived to the U.S. between 2006-2019, natives in 2006-2019", y=NA) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(12)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank()) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  scale_fill_manual(values=c("#FF6F59", "#61D095"))
p <- ggsave("charts/educations.png", plot = p, 
            width = 24, height = 16, units = "cm")


#####################################
## Visualize US location of immigrants
#####################################
## FIPS codes and area titles crosswalk
fips <- read_excel("data/all-geocodes-v2017.xlsx", col_names = TRUE)
fips$state_fips <- as.integer(fips$state_fips)
fips$county_fips <- as.integer(fips$county_fips)

## Immigrant location data - entire sample
all_location <- read_excel("data/ipums_data_for_mapping.xlsx", col_names = TRUE, sheet = "all_locations")
# join location data with fips dataset to bring in county names
all_location_counties <- inner_join(all_location, fips, by = c("state_fips", "county_fips"))

## County map of total immigrants
all_location_counties_all <- all_location_counties %>%
  group_by(state_fips, county_fips) %>%
  filter(row_number()==1) %>%
  mutate(total = log(total))

p <- plot_usmap(regions = "counties", data = all_location_counties_all, values = "total") +
  scale_fill_distiller(palette = "Reds", direction = 1, na.value="white") + labs(title = "Locations of Immigrants, 2006-2019", subtitle = "Among those who arrived to the US in 2006-2019") + 
  theme_classic() + theme(axis.line = element_blank()) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(legend.position="none") + theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p <- ggsave("charts/locations_counties_log_all.png", plot = p, 
            width = 24, height = 16, units = "cm")

## State map of total immigrants
all_location_states <- inner_join(all_location, fips, by = c("state_fips"))

all_location_states_all <- all_location_states %>%
  group_by(state_fips) %>%
  mutate(total = sum(total)) %>%
  filter(row_number()==1) %>%
  select(-c(state_fips, county_fips.x, county_fips.y, counties, fips))

p <- plot_usmap(data = all_location_states_all, values = "total") +
  scale_fill_distiller(palette = "Reds", direction = 1, na.value="white") + labs(title = "Locations of Immigrants, 2006-2019", subtitle = "Among those who arrived to the US in 2006-2019") + 
  theme_classic() + theme(axis.line = element_blank()) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(legend.position="none") + theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p <- ggsave("charts/locations_states_all.png", plot = p, 
            width = 24, height = 16, units = "cm")


## Immigrant location data - comparing locations over time
location_changes <- read_excel("data/ipums_data_for_mapping.xlsx", col_names = TRUE, sheet = "location_changes")
# join location data with fips dataset to bring in county names
location_changes_counties <- inner_join(location_changes, fips, by = c("state_fips", "county_fips"))

# Proportion of counties that are immigrants
p <- plot_usmap(regions = "counties", data = location_changes_counties, values = "prop_imm") +
  scale_fill_distiller(palette = "Reds", direction = 1, labels=function(x) paste0(x, "%")) + 
  labs(title = "Immigrant Share of County Population, 2018") + 
  theme_classic() + theme(axis.line = element_blank()) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(legend.position=c(0.95,0.5), legend.title=element_blank(), legend.text=element_text(size=10)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank())

p <- ggsave("charts/locations_prop_immigrant.png", plot = p, 
            width = 24, height = 16, units = "cm")

location_changes_counties <- location_changes_counties %>%
  filter(imm_change_prop<290)

# Percent changes in immigrant proportion of population
p <- plot_usmap(regions = "counties", data = location_changes_counties, values = "imm_change_prop") +
  scale_fill_gradient2(low="blue", high="red", midpoint=0,
                       labels=function(x) paste0(x, "%")) + 
  labs(title = "Change in Immigrant Share of Population, 2006-2018") + 
  theme_classic() + theme(axis.line = element_blank()) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic")) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(legend.position=c(0.95,0.5), legend.title=element_blank(), legend.text=element_text(size=10)) + 
  theme(legend.margin = margin(0,0,0,0), legend.box.margin = margin(-2, -2, -2, -2)) +
  theme(legend.background = element_blank(), legend.key = element_blank())

p <- ggsave("charts/locations_prop_immigrant_change.png", plot = p, 
            width = 24, height = 16, units = "cm")