## president_approval_post.R
## Last edited 5/11/20
######################################################################################
library(tidyverse)
library(readxl)
library(ggthemes)
library(RColorBrewer)

setwd("C:/Users/trode/Downloads")

approvals <- read_excel("PJAR Raw Data.xlsx", col_names = TRUE)
approvals$enddate <- as.Date(approvals$enddate)
approvals$president <- factor(approvals$president, levels = c("Truman", "Eisenhower", "JFK", "LBJ", "Nixon", "Ford", 
                                                              "Carter", "Reagan", "H.W. Bush", "Clinton", "W. Bush", 
                                                              "Obama", "Trump"))

ggplot(data = approvals) + geom_line(aes(x = daysin, y = approval, color = president))

# unfiltered approval ratings over time
apps_labels <- data.frame(enddate = as.Date(c("1948-01-01","1956-01-01","1962-09-01","1967-01-01","1971-06-01","1975-01-01","1978-06-01","1985-01-01","1990-01-01","1997-01-01","2003-01-01","2013-01-01","2019-07-01")),
                          approval = c(69, 78, 80, 73, 67, 71, 75, 73, 90, 70, 80, 60, 51),
                          label = c("Truman","Eisenhower","JFK","LBJ","Nixon","Ford","Carter","Reagan","H.W. Bush","Clinton","W. Bush","Obama","Trump"))
ggplot(data = approvals) + geom_line(aes(x = enddate, y = approval, color = president)) + 
  geom_text(data = apps_labels, aes(x=enddate, y=approval,label=label, family = "serif", size = 11)) + theme_fivethirtyeight() + 
  theme(legend.position = "none") + labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?", x = NULL) + ylab("Approval Rating") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + scale_y_continuous(labels = c("20%", "40%", "60%", "80%", "100%")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic"))

# add in event markers
ggplot(data = spikes) + geom_line(aes(x = enddate, y = approval, color = president)) + 
  geom_vline(xintercept = as.Date(c("2001-09-11", "1991-01-17", "1973-01-01")), linetype = "dashed", size = 1.2, alpha = .35, color = "red") + 
  annotate(geom = "label", x=as.Date("2001-09-11"), y=18, label = "9/11", size = 4) + 
  annotate(geom = "label", x=as.Date("1991-01-17"), y=18, label = "Gulf War", size = 4) + 
  annotate(geom = "label", x=as.Date("1973-01-01"), y=18, label = "Watergate", size = 4)+ 
  geom_text(data = apps_labels, aes(x=enddate, y=approval,label=label, family = "serif", size = 11)) + theme_fivethirtyeight() + 
  labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?", x = NULL) + ylab("Approval Rating") + theme(legend.position = "none") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + scale_y_continuous(labels = c("20%", "40%", "60%", "80%", "100%")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic"))

# smooth the lines so trends are more apparent - loess method (local regression fitting)
apps_labels2 <- data.frame(enddate = as.Date(c("1949-01-01","1956-01-01","1962-01-01","1967-01-01","1971-03-01","1974-09-01","1978-06-01","1985-01-01","1990-01-01","1996-01-01","2004-01-01","2013-01-01","2019-06-01")),
                          approval = c(56, 75, 78, 70, 64, 70, 73, 67, 82, 65, 80, 55, 49),
                          label = c("Truman","Eisenhower","JFK","LBJ","Nixon","Ford","Carter","Reagan","H.W. Bush","Clinton","W. Bush","Obama","Trump"))
ggplot(data = approvals) + geom_smooth(aes(x = enddate, y = approval, color = president), se = FALSE) + 
  geom_text(data = apps_labels2, aes(x=enddate, y=approval,label=label)) + theme_fivethirtyeight() + 
  labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?", x = NULL) + ylab("Approval Rating") + theme(legend.position = "none") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + scale_y_continuous(labels = c("20%", "40%", "60%", "80%", "100%")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic"))

# boxplot to look at entire distribution
ggplot(data = approvals) + geom_boxplot(aes(x = president, y = approval, fill = president), alpha = 0.85) + theme_fivethirtyeight() + 
  labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?", x = NULL) + ylab("Approval Rating") + theme(legend.position = "none") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + scale_y_continuous(labels = c("20%", "40%", "60%", "80%", "100%")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, size = 11))

# summary info for use in the discussion
avg_app <- approvals %>%
  group_by(president) %>%
  summarise(avg = mean(approval))
ggplot(avg_app) + geom_point(aes(x = president, y = avg))
plot(ecdf(approvals$approval))


# unfiltered approval ratings over time - partisan version
parties <- approvals %>%
  filter(president != "Obama") %>%
  group_by(party, daysin) %>%
  summarise(avg = mean(approval, na.rm = TRUE))
ggplot(data = parties) + geom_smooth(aes(x = daysin, y = avg, color = party)) + theme_fivethirtyeight() + 
  theme(legend.position = "top") + labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?") + ylab("Approval Rating") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic")) + xlab("Days Into Office") + 
  theme(legend.title = element_blank()) + scale_color_manual(values=c('dodgerblue','red2'),labels = c("Democrats", "Republicans"))

# boxplot to look at entire distribution - partisan version
ggplot(data = parties) + geom_boxplot(aes(x = party, y = avg, fill = party), alpha = 0.8) + theme_fivethirtyeight() + 
  labs(title = "Do you approve or disapprove of the way [President name] \n is handling his job as President?", x = NULL) + ylab("Approval Rating") + theme(legend.position = "none") + 
  theme(axis.title = element_text(size = 14, face = "italic")) + scale_y_continuous(labels = c("20%", "40%", "60%", "80%", "100%")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, size = 11)) + scale_fill_manual(values=c('dodgerblue','red2'), labels = c("Democrats", "Republicans"))


