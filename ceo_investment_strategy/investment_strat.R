# investment_strat.R
# Takes historical prices from STATA program and creates line charts.
# Created by Tal Roded on 8/14/18
#############################################################################################################################################
# All necessary libraries
library(readr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(stringr)

windowsFonts(nj1 = windowsFont("Franklin Gothic"))
windowsFonts(nj2 = windowsFont("Lucida Bright"))


# Apple
stock_aapl <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/AAPL1.csv")

aapl <- ggplot(data = stock_aapl, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2011-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray95', color = 'snow2') + 
  geom_rect(xmin = as.Date("1997-01-01"), xmax = as.Date("2011-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') +
  geom_rect(xmin = as.Date("1996-01-01"), xmax = as.Date("1997-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1993-01-01"), xmax = as.Date("1996-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.08, fill = 'gray80', color = 'snow2') +
  geom_rect(xmin = as.Date("1983-01-01"), xmax = as.Date("1993-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.02, fill = 'gray75', color = 'snow3') +
  geom_rect(xmin = as.Date("1981-01-01"), xmax = as.Date("1983-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.02, fill = 'gray70', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Apple Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1981-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("AAPL", "S&P 500 Index"), values = c("darkorange1", "dodgerblue2"))

aapl.labels <- data.frame(
  date=as.Date(c("2014-03-01", "2004-01-01", "1996-07-01", "1994-06-01", "1988-07-01", "1982-06-01")), 
  index = c(280, 120, 65, 40, 23, 10), 
  label = c("Tim Cook", "Steve Jobs", "Gilbert Amelio", "Michael Spindler", "John Sculley", "Mike Markkula"), 
  name = "CEO"
)

aapl + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = aapl.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# Boeing
stock_ba <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/BA1.csv")

ba <- ggplot(data = stock_ba, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2015-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2005-01-01"), xmax = as.Date("2015-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow2') +
  geom_line(size = 0.75) + labs(title = "Boeing Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('2005-01-01', '2018-08-01'))) + 
  scale_color_manual(labels = c("Boeing", "S&P 500 Index"), values = c("#0066B3", "#999B9E"))

ba.labels <- data.frame(
  date=as.Date(c("2016-11-01", "2010-01-01")), 
  index = c(300, 130), 
  label = c("Dennis Muilenburg", "James McNerney"), 
  name = "CEO"
)

ba + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = ba.labels, aes(x = date, y = index, label = label), color = 'black', size = 4, fontface = 'bold') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# Bank of America
stock_bac <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/BAC1.csv")

bac <- ggplot(data = stock_bac, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2010-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2001-01-01"), xmax = as.Date("2010-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1998-01-01"), xmax = as.Date("2001-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Bank of America Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1998-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("BAC", "S&P 500 Index"), values = c("#0067B1", "#EE2A24"))

bac.labels <- data.frame(
  date=as.Date(c("2014-01-01", "2005-07-01", "1999-07-01")), 
  index = c(27, 20, 30), 
  label = c("Brian Moynihan", "Ken Lewis", "Hugh McColl"), 
  name = "CEO"
)

bac + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = bac.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.48) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# Comcast
stock_cmcs <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/CMCSA1.csv")

cmcs <- ggplot(data = stock_cmcs, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("1990-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("1980-03-01"), xmax = as.Date("1990-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_line(size = 0.75) + labs(title = "Comcast Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1980-03-01', '2018-08-01'))) +
  scale_color_manual(labels = c("CMCS.A", "S&P 500 Index"), values = c("#cb1f47", "#000000"))

cmcs.labels <- data.frame(
  date=as.Date(c("2004-01-01", "1985-01-01")), 
  index = c(250, 50), 
  label = c("Brian Roberts", "Ralph Roberts"), 
  name = "CEO"
)

cmcs + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = cmcs.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# Cisco
stock_csco <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/CSCO1.csv")

csco <- ggplot(data = stock_csco, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2014-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("1995-01-01"), xmax = as.Date("2014-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1990-02-01"), xmax = as.Date("1995-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Cisco Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1990-02-01', '2018-08-01'))) +
  scale_color_manual(labels = c("CSCO", "S&P 500 Index"), values = c("#C5112E", "#049FD9"))

csco.labels <- data.frame(
  date=as.Date(c("2016-06-01", "2005-07-01", "1992-09-01")), 
  index = c(600, 500, 50), 
  label = c("Chuck Robbins", "John Chambers", "John Morgridge"), 
  name = "CEO"
)

csco + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = csco.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# Google
stock_goog <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/GOOG1.csv")

goog <- ggplot(data = stock_goog, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2015-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2004-08-01"), xmax = as.Date("2015-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_line(size = 0.75) + labs(title = "Google Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('2004-08-01', '2018-08-01'))) +
  scale_color_manual(labels = c("GOOG", "S&P 500 Index"), values = c("#00A1F1", "#F65314"))

goog.labels <- data.frame(
  date=as.Date(c("2016-07-01", "2010-04-01")), 
  index = c(20, 8), 
  label = c("Sundar Pichai", "Larry Page"), 
  name = "CEO"
)

goog + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = goog.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# Goldman Sachs
stock_gs <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/GS1.csv")

gs <- ggplot(data = stock_gs, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2006-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.035, fill = 'gray80', color = 'snow3') + 
  geom_rect(xmin = as.Date("1999-05-01"), xmax = as.Date("2006-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray75', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Goldman Sachs Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1999-05-01', '2018-08-01'))) +
  scale_color_manual(labels = c("GS", "S&P 500 Index"), values = c("#7399C6", "#FFFFFF"))

gs.labels <- data.frame(
  date=as.Date(c("2011-09-01", "2002-09-01")), 
  index = c(3, 1.75), 
  label = c("Lloyd Blankfein", "Henry Paulson"), 
  name = "CEO"
)

gs + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = gs.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# IBM
stock_ibm <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/IBM1.csv")

ibm <- ggplot(data = stock_ibm, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2012-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray100', color = 'snow2') + 
  geom_rect(xmin = as.Date("2002-01-01"), xmax = as.Date("2012-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray95', color = 'snow2') +
  geom_rect(xmin = as.Date("1993-01-01"), xmax = as.Date("2002-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') +
  geom_rect(xmin = as.Date("1985-01-01"), xmax = as.Date("1993-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1981-01-01"), xmax = as.Date("1985-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_rect(xmin = as.Date("1973-01-01"), xmax = as.Date("1981-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray75', color = 'snow3') +
  geom_rect(xmin = as.Date("1971-01-01"), xmax = as.Date("1973-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.035, fill = 'gray70', color = 'snow3') +
  geom_rect(xmin = as.Date("1962-01-01"), xmax = as.Date("1971-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray65', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "IBM Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1962-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("IBM", "S&P 500 Index"), values = c("#1F70C1", "#000000"))

ibm.labels <- data.frame(
  date=as.Date(c("2016-01-01", "2007-01-01", "1997-07-01", "1989-01-01", "1983-01-1", "1977-01-01", "1972-01-01", "1966-07-01")), 
  index = c(15, 26, 24, 10, 7.5, 6, 4.5, 3), 
  label = c("Virginia Rometty", "Sam Palmisano", "Louis Gerstner", "John Akers","John Opel","Frank Cary","T. Learson","Thomas Watson Jr."), 
  name = "CEO"
)

ibm + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = ibm.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.25) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# Intel
stock_intc <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/INTC1.csv")

intc <- ggplot(data = stock_intc, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2018-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray100', color = 'snow2') + 
  geom_rect(xmin = as.Date("2013-01-01"), xmax = as.Date("2018-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray90', color = 'snow2') +
  geom_rect(xmin = as.Date("2005-01-01"), xmax = as.Date("2013-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1998-01-01"), xmax = as.Date("2005-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_rect(xmin = as.Date("1987-01-01"), xmax = as.Date("1998-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray75', color = 'snow3') +
  geom_rect(xmin = as.Date("1980-03-01"), xmax = as.Date("1987-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray70', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Intel Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1980-03-01', '2018-08-01'))) +
  scale_color_manual(labels = c("INTC", "S&P 500 Index"), values = c("#127BCA", "#FFFFFF"))

intc.labels <- data.frame(
  date=as.Date(c("2018-04-01", "2015-06-01", "2009-01-01", "2001-02-01", "1992-01-01", "1983-07-01")), 
  index = c(180, 135, 95, 170, 30, 10), 
  label = c("Bob Swan", "Brian Krzanich", "Paul Otellini","Craig Barrett","Andrew Grove","Gordon Moore"), 
  name = "CEO"
)

intc + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = intc.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# JP Morgan
stock_jpm <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/JPM1.csv")

jpm <- ggplot(data = stock_jpm, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2007-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2000-01-01"), xmax = as.Date("2007-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1995-01-01"), xmax = as.Date("2000-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray80', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "JP Morgan Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1995-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("JPM", "S&P 500 Index"), values = c("#154C8A", "#000000"))

jpm.labels <- data.frame(
  date=as.Date(c("2010-07-01", "2004-01-01", "1997-07-01")), 
  index = c(18, 14, 15), 
  label = c("Jamie Dimon", "William Harrison", "Douglas Warner"), 
  name = "CEO"
)

jpm + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = jpm.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# McDonald's
stock_mcd <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/MCD1.csv")

mcd <- ggplot(data = stock_mcd, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2015-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray100', color = 'snow2') + 
  geom_rect(xmin = as.Date("2012-01-01"), xmax = as.Date("2015-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray90', color = 'snow2') +
  geom_rect(xmin = as.Date("2004-01-01"), xmax = as.Date("2012-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("2003-01-01"), xmax = as.Date("2004-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_rect(xmin = as.Date("1999-01-01"), xmax = as.Date("2003-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray75', color = 'snow3') +
  geom_rect(xmin = as.Date("1987-01-01"), xmax = as.Date("1999-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray70', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "McDonald's Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1987-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("MCD", "S&P 500 Index"), values = c("#FF2D08", "#FFF10A"))

mcd.labels <- data.frame(
  date=as.Date(c("2016-07-01", "2013-06-01", "2007-01-01", "2003-07-01", "2001-01-01", "1993-01-01")), 
  index = c(450, 360, 225, 120, 170, 100), 
  label = c("Steve Easterbrook", "Don Thompson", "Jim Skinner","Jim Cantalupo","Jack Greenberg","Michael Quinlan"), 
  name = "CEO"
)

mcd + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = mcd.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# Amazon
stock_amzn <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/AMZN1.csv")

amzn <- ggplot(data = stock_amzn, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("1997-05-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_line(size = 0.75) + labs(title = "Amazon Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1997-05-01', '2018-08-01'))) +
  scale_color_manual(labels = c("AMZN", "S&P 500 Index"), values = c("darkorange1", "dodgerblue2"))

amzn.labels <- data.frame(
  date=as.Date(c("2007-07-01")), 
  index = c(500), 
  label = c("Jeff Bezos"), 
  name = "CEO"
)

amzn + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = amzn.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# 3M
stock_mmm <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/MMM1.csv")

mmm <- ggplot(data = stock_mmm, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2018-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray100', color = 'snow2') + 
  geom_rect(xmin = as.Date("2012-01-01"), xmax = as.Date("2018-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray90', color = 'snow2') +
  geom_rect(xmin = as.Date("2005-01-01"), xmax = as.Date("2012-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("2001-01-01"), xmax = as.Date("2005-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray80', color = 'snow3') +
  geom_rect(xmin = as.Date("1991-01-01"), xmax = as.Date("2001-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray75', color = 'snow3') +
  geom_rect(xmin = as.Date("1986-01-01"), xmax = as.Date("1991-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray70', color = 'snow3') +
  geom_rect(xmin = as.Date("1979-01-01"), xmax = as.Date("1986-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray65', color = 'snow3') +
  geom_rect(xmin = as.Date("1974-01-01"), xmax = as.Date("1979-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray60', color = 'snow3') +
  geom_rect(xmin = as.Date("1970-01-01"), xmax = as.Date("1974-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.03, fill = 'gray55', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "3M Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1970-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("MMM", "S&P 500 Index"), values = c("#FD000D", "#000000"))

mmm.labels <- data.frame(
  date=as.Date(c("2018-04-01", "2015-01-01", "2008-07-01", "2003-01-01", "1995-01-01", "1988-06-01", "1982-06-01", "1976-06-01", "1972-01-01")), 
  index = c(40, 31, 22, 19, 15, 7, 5, 3, 5), 
  label = c("Michael Roman", "Inge Thulin", "George Buckley", "W. McNerney","L.D. DeSimone","Allen Jacobson","Lewis Lehr", "Raymond Herzog", "Harry Heltzer"), 
  name = "CEO"
)

mmm + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = mmm.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))




# Microsoft
stock_msft <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/MSFT1.csv")

msft <- ggplot(data = stock_msft, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2014-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2000-01-01"), xmax = as.Date("2014-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_rect(xmin = as.Date("1986-03-01"), xmax = as.Date("2000-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.05, fill = 'gray80', color = 'snow3') +
  geom_line(size = 0.75) + labs(title = "Microsoft Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1986-03-01', '2018-08-01'))) +
  scale_color_manual(labels = c("MSFT", "S&P 500 Index"), values = c("#00A1F1", "#F65314"))

msft.labels <- data.frame(
  date=as.Date(c("2016-01-01", "2007-01-01", "1993-01-01")), 
  index = c(750, 450, 150), 
  label = c("Satya Nadella", "Steve Ballmer", "Bill Gates"), 
  name = "CEO"
)

msft + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = msft.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# PepsiCo
stock_pep <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/PEP1.csv")

pep <- ggplot(data = stock_pep, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2006-01-01"), xmax = as.Date("2018-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray90', color = 'snow2') + 
  geom_rect(xmin = as.Date("2001-01-01"), xmax = as.Date("2006-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'gray85', color = 'snow2') +
  geom_line(size = 0.75) + labs(title = "PepsiCo Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('2001-01-01', '2018-08-01'))) +
  scale_color_manual(labels = c("PEP", "S&P 500 Index"), values = c("#004A98", "#EC0928"))

pep.labels <- data.frame(
  date=as.Date(c("2012-01-01", "2003-06-01")), 
  index = c(58, 39), 
  label = c("Indra Nooyi", "Steven Reinemund"), 
  name = "CEO"
)

pep + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") +
  geom_text(data = pep.labels, aes(x = date, y = index, label = label), color = 'black', fontface = 'bold', size = 3.5) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.2, 0.9))



# All Stocks
stock_all <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/allstocks.csv")

all_stock <- ggplot(data = stock_all, aes(x = date, y = index, group=name, color=name)) + 
  geom_line(size = 0.75) + labs(title = "Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1984-01-01', '2018-08-01')))

all_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))



# Tech Stocks
stock_tech <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/techstocks.csv")

tech_stock <- ggplot(data = stock_tech, aes(x = date, y = index, group=name, color=name)) + 
  geom_line(size = 0.75) + labs(title = "Tech Sector Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1987-01-01', '2018-08-01'))) + 
  scale_color_brewer(palette = "Set1")

tech_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))



# Industrial Stocks
stock_ind <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/industrialstocks.csv")

ind_stock <- ggplot(data = stock_ind, aes(x = date, y = index, group=name, color=name)) + 
  geom_line(size = 0.75) + labs(title = "Industrial Sector Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1987-01-01', '2018-08-01'))) + 
  scale_color_brewer(palette = "Set1")

ind_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))


# Financial Stocks
stock_fin <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/financialstocks.csv")

fin_stock <- ggplot(data = stock_fin, aes(x = date, y = index, group=name, color=name)) + 
  geom_line(size = 0.75) + labs(title = "Financial Sector Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1987-01-01', '2018-08-01'))) + 
  scale_color_brewer(palette = "Set1")

fin_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))



# Financial Stocks Financial Crisis
stock_fin <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/financialstocks.csv")

fin_stock <- ggplot(data = stock_fin, aes(x = date, y = index, group=name, color=name)) + 
  geom_rect(xmin = as.Date("2007-09-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf, alpha = 0.015, fill = 'gray', color = 'snow2') +
  geom_line(size = 0.75) + labs(title = "Financial Sector Great Recession Performance") +
  scale_x_date(date_breaks="2 years", date_labels="%Y", limits=as.Date(c('2007-01-01', '2018-08-01'))) + 
  scale_color_brewer(palette = "Set1")
 

fin_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))



# Food Stocks
stock_food <- read_csv("C:/Users/C1TGR01/Desktop/Blog Post Components/InvestmentStrat_Post/foodstocks.csv")

food_stock <- ggplot(data = stock_food, aes(x = date, y = index, group=name, color=name)) + 
  geom_line(size = 0.75) + labs(title = "Food Industry Historical Performance") +
  scale_x_date(date_breaks="5 years", date_labels="%Y", limits=as.Date(c('1987-01-01', '2018-08-01'))) + 
  scale_color_brewer(palette = "Set1")

food_stock + theme_fivethirtyeight() + theme(axis.title = element_text()) + ylab("Index Value") + xlab("Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.3, 0.9))
