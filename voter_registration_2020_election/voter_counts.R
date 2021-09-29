## voter_counts.R
## Last edited by Tal Roded, 10/22/20
######################################
library(tidyverse)
library(readxl)
library(ggthemes)
library(RColorBrewer)
library(Cairo)

setwd("C:/Users/trode/Desktop/VisualizeCuriosity/voter_counts")

#######################
## PA Counts
#######################
####
## Compare October voter counts to June (primary) voter counts - has either party gained an edge?
####
pa_oct_count <- read_excel("pa_voter_counts.xlsx", col_names = TRUE, sheet = "Reg Voter")
pa_oct_count <- pa_oct_count %>%
  mutate(thirdParty = no_party_voters + other_voters, 
         date = "October count")

pa_jun_count <- read_excel("pa_voter_counts.xlsx", col_names = TRUE, sheet = "jun_reg_voter")
pa_jun_count <- pa_jun_count %>%
  mutate(date = "June count")

pa_16_count <- read_excel("pa_voter_counts.xlsx", col_names = TRUE, sheet = "last_reg_voter")
pa_16_count <- pa_16_count %>%
  mutate(date = "2016 count")

pa_counts <- bind_rows(pa_oct_count, pa_jun_count)

pa_counts_long <- pa_counts %>%
  gather(Party, Count, c(dem_voters, rep_voters, thirdParty)) %>%
  filter(County=="Total")

CairoPNG(filename = "philly_counts_ts.png", width = 640, height = 480)
ggplot(pa_counts_long) + geom_bar(aes(x=Party, y=Count, fill=date), color="black", stat="identity", position = position_dodge())+ 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_fill_manual(values = c("#C3DFE0", "#BCD979"), labels = c("June 2   ", "October 19")) + 
  theme(axis.text = element_text(size=12)) + scale_x_discrete(labels = c("Democrats", "Republicans", "Other Party")) + 
  scale_y_continuous(labels = c("0", "1", "2", "3", "4")) + labs(y = "Count  (millions) \n", title = "Voter Registrations in Pennsylvania") + 
  theme(axis.title.y = element_text(size=14, face="bold")) + theme(plot.title = element_text(hjust=0.5, size=20))
dev.off()

pa_counts_16 <- bind_rows(pa_16_count, pa_jun_count)

pa_counts_long_16 <- pa_counts_16 %>%
  gather(Party, Count, c(dem_voters, rep_voters, thirdParty)) %>%
  filter(County=="Total")

CairoPNG(filename = "philly_counts_ts_2016.png", width = 640, height = 480)
ggplot(pa_counts_long_16) + geom_bar(aes(x=Party, y=Count, fill=date), color="black", stat="identity", position = position_dodge())+ 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_fill_manual(values = c("#C3DFE0", "#BCD979"), labels = c("2016 Election   ", "October 19")) + 
  theme(axis.text = element_text(size=12)) + scale_x_discrete(labels = c("Democrats", "Republicans", "Other Party")) + 
  scale_y_continuous(labels = c("0", "1", "2", "3", "4")) + labs(y = "Count  (millions) \n", title = "Voter Registrations in Pennsylvania") + 
  theme(axis.title.y = element_text(size=14, face="bold")) + theme(plot.title = element_text(hjust=0.5, size=20))
dev.off()

####
## Compare R vs D voter counts by age - is one group particularly younger/older than the other? Which age brackets
##  does each party have advantage in?
####
pa_voters_age_d <- read_excel("pa_voter_counts.xlsx", col_names = TRUE, sheet = "Dem by Age")
pa_voters_age_r <- read_excel("pa_voter_counts.xlsx", col_names = TRUE, sheet = "Rep by Age")

pa_age <- bind_rows(pa_voters_age_d, pa_voters_age_r)

## Raw Count Totals
pa_age_long_counts <- pa_age %>%
  gather(Age, Count, c("18 to 24":"75+")) %>%
  filter(County=="Totals")

CairoPNG(filename = "philly_ages.png", width = 640, height = 480)
ggplot(pa_age_long_counts) + geom_bar(aes(x=Age, y=Count, fill=Party), color="black", stat="identity", position = position_dodge()) + 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_fill_brewer(palette = "Set1", labels = c("Democrats", "Republicans"), direction = -1) + 
  theme(axis.text = element_text(size=12)) + labs(y = "Thousands \n", title = "PA Voter Registrations by Age Groups") + 
  theme(axis.title.y = element_text(size=14, face="bold")) + theme(plot.title = element_text(hjust=0.5, size=20)) + 
  scale_y_continuous(labels = c("0", "200", "400", "600", "800"))
dev.off()

## Percents
pa_age_long_per <- pa_age %>%
  gather(Age, Count, c("18 to 24":"75+")) %>%
  filter(County=="Percents")

CairoPNG(filename = "philly_ages_perc.png", width = 640, height = 480)
ggplot(pa_age_long_per) + geom_bar(aes(x=Age, y=Count, fill=Party), color="black", stat="identity", position = position_dodge()) + 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_fill_brewer(palette = "Set1", labels = c("Democrats", "Republicans"), direction = -1) + 
  theme(axis.text = element_text(size=12)) + labs(title = "PA Voter Registrations by Age Groups") + 
  theme(plot.title = element_text(hjust=0.5, size=20)) + scale_y_continuous(labels = c("0%", "5%", "10%", "15%", "20%"))
dev.off()

#######################
## FL Counts
#######################
x <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
for (val in x) {
  nam <- paste("fl_voters_", val, sep = "")
  assign(nam, read_excel("fl_voter_counts.xlsx", col_names = TRUE, sheet = val, skip = 3))
  #assign(nam, mutate(paste("fl_voters_",val,"$","thirdParty", sep = "") = paste("fl_voters_",val,"$","`Minor Party`", sep = "") + paste("fl_voters_",val,"$","`No Party Affiliation`", sep = "")))
}

fl_ts <- read_excel("fl_voter_counts.xlsx", col_names = TRUE, sheet = "cleaned")

fl_ts_long <- fl_ts %>%
  gather(Party, Count, c(republican, xdemocrat, znone)) %>%
  mutate(Count = Count/1000000)

fl_ts_long$date <- as.Date(fl_ts_long$date)

CairoPNG(filename = "fl_ts.png", width = 640, height = 480)
ggplot(fl_ts_long) + geom_line(aes(x=date, y=Count, color=Party), size=1) + 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_color_brewer(palette = "Set1", labels = c("Republican", "Democrat", "None")) + 
  theme(axis.text = element_text(size=12)) + labs(y = "Millions \n", title = "Voter Registrations in Florida") + 
  theme(axis.title.y = element_text(size=14, face="bold")) + theme(plot.title = element_text(hjust=0.5, size=20)) + 
  ylim(3.5, 5.5) + scale_x_date(date_labels = "%b-%y", date_breaks = "1 month")
dev.off()


#######################
## NC Counts
#######################
####
nc_count <- read_excel("nc_voter_counts.xlsx", col_names = TRUE)

nc_count_long <- nc_count %>%
  gather(Party, Count, c(xdem_voter, rep_voter, zother_voter))

nc_count_long$date <- factor(nc_count_long$date, levels = c("November 2016", "March Primary", "October 17"))

CairoPNG(filename = "nc_counts.png", width = 640, height = 480)
ggplot(nc_count_long) + geom_bar(aes(x=date, y=Count, fill=Party), color="black", stat="identity", position = position_dodge())+ 
  theme_fivethirtyeight() + theme(legend.title = element_blank()) + theme(legend.position = "top") + theme(legend.text = element_text(size=14)) + 
  scale_fill_brewer(palette = "Set1", labels = c("Republican", "Democrat", "Other"))+ 
  theme(axis.text = element_text(size=12)) + scale_x_discrete(labels = c("November 2016", "March 2020 - Primary", "October 17, 2020")) + 
  scale_y_continuous(labels = c("0", "1", "2", "3")) + labs(y = "Count  (millions) \n", title = "Voter Registrations in North Carolina") + 
  theme(axis.title.y = element_text(size=14, face="bold")) + theme(plot.title = element_text(hjust=0.5, size=20))
dev.off()















