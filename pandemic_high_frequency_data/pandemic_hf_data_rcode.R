## Visualize Curiosity Project - Charting the Effects of a Pandemic w/ HF Data
## Last edited by Tal Roded, 4/7/20
####################################################
library(tidyverse)
library(readxl)
library(ggthemes)

library(RColorBrewer)
library(ggthemes)
setwd("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/Pandemic_High_Frequency_Data")

## Load in Homebase data
homebase1 <- read_excel("homebase-hourly-employees-income.xlsx", col_names = TRUE, skip = 1)

## Hours worked by hourly employees chart
hrs_wk_adj <- homebase1$hrs_worked*100
ggplot(data = homebase1) + geom_line(aes(x=Date, y = hrs_wk_adj), size = 1.2, color = "#fb8072") + 
  geom_abline(slope = 0, size = 1) + 
  labs(title = "Change in hours worked by hourly employees", x = NULL) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size = 10, face = "italic")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text()) + ylab("% Change") + 
  geom_label(x=as.numeric(homebase1$Date[9]), y=-50, label = "March 13 \n National Emergency Declared", size = 3) + 
  geom_vline(aes(xintercept = as.numeric(Date[13])), linetype = 2)

## Number of employees working chart  
emp_wk_adj <- homebase1$emps_working*100
ggplot(data = homebase1) + geom_line(aes(x=Date, y = emp_wk_adj), size = 1.2, color = "#8dd3c7") + 
  geom_abline(slope = 0, size = 1) + 
  labs(title = "Change in number of hourly employees working", x = NULL) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size = 10, face = "italic")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text()) + ylab("% Change") + 
  geom_label(x=as.numeric(homebase1$Date[9]), y=-50, label = "March 13 \n National Emergency Declared", size = 3) + 
  geom_vline(aes(xintercept = as.numeric(Date[13])), linetype = 2)

## Combine the above two plots
ggplot(data = homebase1) + geom_line(aes(x=Date, y = hrs_wk_adj), size = 1.2, color = "#fb8072") + 
  geom_line(aes(x=Date, y = emp_wk_adj), size = 1.2, color = "#8dd3c7") + 
  geom_abline(slope = 0, size = 1) + 
  labs(title = "Hours Worked vs. Number Working", x = NULL) + 
  theme_fivethirtyeight() + theme(axis.text = element_text(size = 10, face = "italic")) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text()) + ylab("% Change") + 
  geom_label(x=as.numeric(homebase1$Date[9]), y=-50, label = "March 13 \n National Emergency Declared", size = 3) + 
  geom_vline(aes(xintercept = as.numeric(Date[13])), linetype = 2)


# OpenTable data - change in restaurant reservations and walk-ins YoY
## load in OpenTable data and create base map
open_table <- read_excel("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/Pandemic_High_Frequency_Data/state_of_industry_data.xlsx")
all_states <- map_data('state')
ggplot(data = all_states) + geom_polygon(aes(x = long, y = lat, group = group), color = "white") + coord_map() + theme_void()
locations_map <- inner_join(all_states, open_table, by = "region")

## descriptive statistics
open_table %>%
  filter(mar_one<101) %>%
  summary()

## March 1 map
ggplot(data = locations_map) + coord_map() + geom_polygon(aes(x = long, y = lat, group = region, fill = mar_one), color = "light gray") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-100, 100), labels = c("-100%", "-50%", "0%", "+50%", "+100%")) + labs(title = "Change in Seated Diners", subtitle = "On March 1, 2020, compared to March 1, 2019") + 
  theme_classic() + theme(axis.line = element_blank()) + theme(legend.text = element_text(vjust = 0.2)) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.key.width=unit(2,"cm")) + theme(legend.justification = 0.35) + theme(legend.title = element_blank()) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 11))

## March 15 map
ggplot(data = locations_map) + coord_map() + geom_polygon(aes(x = long, y = lat, group = region, fill = mar_ide), color = "gray") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-100, 100), labels = c("-100%", "-50%", "0%", "+50%", "+100%")) + labs(title = "Change in Seated Diners", subtitle = "On March 15, 2020, compared to March 15, 2019") + 
  theme_classic() + theme(axis.line = element_blank()) + theme(legend.text = element_text(vjust = 0.2)) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.key.width=unit(2,"cm")) + theme(legend.justification = 0.35) + theme(legend.title = element_blank()) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 11))

## March 30 map
ggplot(data = locations_map) + coord_map() + geom_polygon(aes(x = long, y = lat, group = region, fill = mar_end), color = "light gray") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-100, 100), labels = c("-100%", "-50%", "0%", "+50%", "+100%")) + labs(title = "Change in Seated Diners", subtitle = "On March 31, 2020, compared to March 31, 2019") + 
  theme_classic() + theme(axis.line = element_blank()) + theme(legend.text = element_text(vjust = 0.2)) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.key.width=unit(2,"cm")) + theme(legend.justification = 0.35) + theme(legend.title = element_blank()) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 11))



# Stock Market Indices
## load in data
indices <- read_excel("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/Pandemic_High_Frequency_Data/market_indices.xlsx")

## Plot 3 stock market indices
inds <- ggplot(data = indices) + geom_line(aes(x = Date, y = Change, group = Index, color = Index), size = 1.05) + 
  theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = "top") + 
  theme(axis.title.x = element_blank()) + labs(title = "Market Indices", subtitle = "Jan 2, 2020 - March 30, 2020") + theme(legend.position = c(0.15, 0.45)) + 
  theme(legend.text = element_text(size = 10)) + scale_color_discrete(limits = c("SP500", "AWAY", "JETS")) + 
  theme(legend.direction = "vertical") + geom_vline(aes(xintercept = as.numeric(Date[16])), linetype = 2) + 
  theme(plot.subtitle = element_text(hjust = 0.5))
inds + geom_label(x=as.numeric(indices$Date[16]), y=36.43, label = "First US COVID-19 Case", size = 3)

## Add hourly employees series from before to market indices chart
hrs_wk_adj2 <- hrs_wk_adj+100
opentable_usa <- read_excel("C:/Users/C1TGR01/Desktop/VisualizeCuriosity/Pandemic_High_Frequency_Data/opentable_usa.xlsx")
inds + geom_line(data = homebase1, aes(x=Date, y = hrs_wk_adj2, color = "red"), color = "#7a0177", linetype = 5, size = 1.1) + 
  geom_line(data = opentable_usa, aes(x=date, y = change, color = "purple"), color = "#253494", linetype = 5, size = 1.1) + 
  geom_label(x=as.numeric(indices$Date[16]), y=-3.1, label = "First US COVID-19 Case", size = 3) + 
  labs(title = "Market Indices + Homebase & OpenTable Data")
  
