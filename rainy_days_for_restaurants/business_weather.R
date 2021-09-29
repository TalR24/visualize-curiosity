## business_weather.R
## Last edited 8/20/20
######################################################################################
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(Cairo)
library(lubridate)

setwd("C:/Users/trode/Desktop/business_weather")

## Load in mobility data
mobility <- read_excel("mobility_report_US.xlsx", col_names = TRUE)
mobility$date <- as.Date(mobility$date)

## Philly Mobility
philly_mobility <- mobility %>%
  filter(county == "Philadelphia County") %>%
  select(-residential_traffic, -transit_traffic, -work_traffic, -state, -county) %>%
  gather(destination, traffic, retail_rec_traffic:parks_traffic)

CairoPNG(filename = "philly_mobility_trends.png", width = 640, height = 480)
ggplot(philly_mobility) + geom_line(aes(x=date, y=traffic, color=destination), size=0.9) + 
  theme_fivethirtyeight() + scale_x_date(date_labels = "%b") + 
  theme(legend.position = "top", legend.title = element_blank()) + 
  scale_color_manual(values = c("#BD9391", "#87D68D", "#F5BB00"), 
                     labels = c("Grocery & Pharma", "Parks", "Retail & Recreation")) + 
  labs(title = "Philly Mobility Trends", subtitle = "Relative to Jan 3-Feb 6 baseline day", y="% Change") + 
  theme(plot.title = element_text(hjust=0.5, size=26), plot.subtitle = element_text(hjust=0.5, face="italic")) + 
  theme(axis.title.y = element_text(face="italic")) + ylim(-75, 75)
dev.off()

# smooth traffic data - good for general trends, bad for examining effect of weather
CairoPNG(filename = "philly_mobility_trends_smooth.png", width = 640, height = 480)
ggplot(philly_mobility) + geom_smooth(aes(x=date, y=traffic, color=destination), size=1, se=FALSE) + 
  theme_fivethirtyeight() + scale_x_date(date_labels = "%b") + 
  theme(legend.position = "top", legend.title = element_blank()) + 
  scale_color_manual(values = c("#BD9391", "#87D68D", "#F5BB00"), 
                     labels = c("Grocery & Pharma", "Parks", "Retail & Recreation")) + 
  labs(title = "Philly Mobility Trends", subtitle = "Relative to Jan 3-Feb 6 baseline day", y="% Change") + 
  theme(plot.title = element_text(hjust=0.5, size=26), plot.subtitle = element_text(hjust=0.5, face="italic")) + 
  theme(axis.title.y = element_text(face="italic")) + ylim(-75, 75)
dev.off()

## OpenTable data
opentable <- read_excel("philly_opentable.xlsx", col_names = TRUE)
opentable$date <- as.Date(opentable$date)

ggplot(opentable) + geom_line(aes(x=date, y=traffic))

df <- rbind(philly_mobility, opentable)

CairoPNG(filename = "philly_mobility_trends_plusdiners.png", width = 640, height = 480)
ggplot(df) + geom_line(aes(x=date, y=traffic, color=destination), size=0.9) + 
  theme_fivethirtyeight() + scale_x_date(date_labels = "%b") + 
  theme(legend.position = "top", legend.title = element_blank()) + 
  scale_color_manual(values = c("red", "#BD9391", "#87D68D", "#F5BB00"), 
                     labels = c("Restaurants", "Grocery & Pharma", "Parks", "Retail & Recreation")) + 
  labs(title = "Philly Mobility & Diners Trends", y="% Change") + 
  theme(plot.title = element_text(hjust=0.5, size=26), plot.subtitle = element_text(hjust=0.5, face="italic")) + 
  theme(axis.title.y = element_text(face="italic")) + ylim(-100, 80)
dev.off()


## Philly Weather
weather <- read_excel("philly_weather.xlsx", col_names = TRUE)
weather$date <- as.Date(weather$date)
weather$startrain <- as.Date(weather$startrain)
weather$endrain <- as.Date(weather$endrain)
weather <- weather %>%
  mutate(rain = ifelse(rain==.01, 0, rain)) %>%
  mutate(rain_ind = ifelse(rain>0, 1, 0))

ggplot(weather) + geom_line(aes(x=date, y=avg_temp))

CairoPNG(filename = "weather_and_trends.png", width = 640, height = 480)
ggplot() + geom_line(data=df, aes(x=date, y=traffic, color=destination), size=1.25) + 
  geom_rect(data=weather, aes(xmin=startrain, xmax=endrain, ymin=-Inf, ymax=+Inf), fill='blue', alpha=0.15) + 
  theme_fivethirtyeight() + scale_x_date(date_labels = "%b") + 
  theme(legend.position = "top", legend.title = element_blank()) + 
  scale_color_manual(values = c("red", "#BD9391", "#87D68D", "#F5BB00"), 
                     labels = c("Restaurants", "Grocery & Pharma", "Parks", "Retail & Recreation")) + 
  labs(title = "Philly Mobility & Diners Trends", y="% Change") + 
  theme(plot.title = element_text(hjust=0.5, size=26), plot.subtitle = element_text(hjust=0.5, face="italic")) + 
  theme(axis.title.y = element_text(face="italic")) + ylim(-100, 80)
dev.off()


## Analysis

opentable <- opentable %>%
  select(-date)
raincomp <- cbind(weather, opentable)

## After shutdowns 
raincomp2 <- raincomp %>%
  select(date, rain_ind, rain, traffic) %>%
  filter(date > "2020-06-15")
raincomp2$rain_ind <- as.character(raincomp2$rain_ind)
#boxplot comparison
ggplot(raincomp2, aes(x=rain_ind, y=traffic)) + geom_boxplot()

rain <- raincomp2 %>%
  filter(rain_ind == 1)
norain <- raincomp2 %>%
  filter(rain_ind == 0)
#difference-of-mean tests
t.test(rain$rain, norain$rain)
wilcox.test(rain$traffic, norain$traffic)

#regression
reg <- lm(raincomp2$traffic ~ raincomp2$rain_ind)
plot(reg, las = 1)
summary(reg)


## Before shutdowns
raincomp3 <- raincomp %>%
  select(date, rain_ind, rain, traffic) %>%
  filter(date < "2020-03-15")
raincomp3$rain_ind <- as.character(raincomp3$rain_ind)
#boxplot comparison
ggplot(raincomp3, aes(x=rain_ind, y=traffic)) + geom_boxplot()

rain <- raincomp3 %>%
  filter(rain_ind == 1)
norain <- raincomp3 %>%
  filter(rain_ind == 0)
#difference-of-mean tests
t.test(rain$rain, norain$rain)
wilcox.test(rain$traffic, norain$traffic)

#regression
reg2 <- lm(raincomp3$traffic ~ raincomp3$rain_ind)
plot(reg2, las = 1)
summary(reg2)




