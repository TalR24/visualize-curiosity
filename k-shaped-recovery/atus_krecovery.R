## atus_krecovery.R
## Code for "A K-Shaped Recovery in Time" Visualize Curiosity post
## Last edited by Tal Roded, 12/5/21
######################################
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(doBy)

setwd("C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/atus_krecovery")


## Chart with stock market and employment numbers comparison
stock <- read_excel("data/sp500.xlsx", col_names = TRUE)
stock$date <- as.character(stock$date)
# note this is total non-farm, seasonally adjusted
emp <- read_excel("data/PAYEMS.xls", col_names = TRUE)
emp$date <- as.character(emp$date)
emp <- emp %>%
  # focus only on 2020 numbers
  filter(date > "2020-01-01")
stock_emp <- inner_join(stock, emp, by = "date")

stock_emp <- stock_emp %>%
  mutate(sp500_relat = 100*(sp500_index/sp500_index[1])) %>%
  mutate(emp_relat = 100*(employed/employed[1])) %>%
  mutate(date = as.Date.character(date)) %>%
  select(date, sp500_relat, emp_relat) %>%
  pivot_longer(!date, names_to="type", values_to="count")
  
# line plot comparing S&P 500 performance with employment - relative changes to Feb 2020
ggplot() + geom_line(data=stock_emp, aes(x=date, y=count, color=type), size=1.6) + 
  geom_hline(yintercept=100, size=1.2) + 
  theme_fivethirtyeight() + theme(axis.title.y = element_text(size=14, face="bold", vjust=1.25)) + 
  labs(title="Comparison of S&P 500 Index and Employment in 2020", y="Relative to Feb 2020") + 
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
  theme(legend.position="top", legend.title=element_blank()) + 
  scale_y_continuous(limits=c(80, 130)) + scale_x_date(date_breaks="1 month", date_labels="%b") + 
  scale_color_manual(values=c("dark red", "dark green"), labels=c("Employment", "S&P 500"))
ggsave("charts/emp_stocks.png")


## Read in ATUS 2019 and 2020 data - respondent file merged with activity summary file
## I manually merged the two files based on tucaseid variable, generated yearmth date variable from tuyear, tumonth
atus_19 <- read_excel("data/clean_atus-2019.xlsx", col_names = TRUE)
atus_19 <- atus_19 %>%
  mutate(dataset="2019")
atus_20 <- read_excel("data/clean_atus-2020.xlsx", col_names = TRUE)
atus_20 <- atus_20 %>%
  mutate(dataset="2020")

# clean the data
atus <- atus %>%
  # only employed people - since we need to pull income
  filter(telfs==1 | telfs==2) %>%
  # create yearly income by multiplying weekly earn * weeks worked
  mutate(income = teern*teernwkp)

# need to replace x/y/z with the activities I want to summarize here
atus_sum <- atus %>%
  summaryBy(x + y + z ~ income, FUN = c(mean, median))

# I should probably hold off on combining the datasets until they're cleaned and smaller
atus_19_20 <- rbind(atus_19, atus_20)
