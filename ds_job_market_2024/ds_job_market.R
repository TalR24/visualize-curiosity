## ds_job_market.R
## Last updated by Tal Roded, 1/4/24
## For the article: What is the future of the data science job market?
################################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

setwd('C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/ds_job_market')


###################################
## BLS Employment Projections
##################################
#df <- read.csv("National Employment Matrix_OCC_15-2051.csv")
df <- read_excel('bls_emp_projection.xlsx')
  
ggplot(df, aes(x = year, y = emp_level, fill=emp_level)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = round(emp_level, 1)), color="black", vjust=-0.5, position=position_dodge(0.9), size=4, fontface='bold') + 
  theme_fivethirtyeight() + theme(legend.position='none') + 
  theme(axis.title.y = element_text(size=14, face='bold'), 
        axis.text.x = element_text(size=12, face='bold'), axis.text.y=element_blank()) + 
  labs(title="Data Scientist Projected Employment Level", y='# Employed (Thousands)') + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  scale_x_continuous(breaks=seq(2022,2032,2)) + 
  #scale_y_continuous(limits=c(150,250)) + 
  scale_fill_gradient(low='pink',high='maroon')
ggsave('charts/data_growth_time.png', width = 26, height = 16, units = "cm")
