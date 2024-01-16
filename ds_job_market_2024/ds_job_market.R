## ds_job_market.R
## Last updated by Tal Roded, 1/11/24
## For the article: What is the future of the data science job market?
################################################################################
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(reshape)
library(scales)
library(usmap)

setwd('C:/Users/trode/OneDrive/Desktop/VisualizeCuriosity/ds_job_market')

###################################
## BLS Employment Projections
###################################
## Yearly emp growth for DS
df <- read_excel('data/bls_emp_projection.xlsx')

# Overall DS employment growth
ggplot(df, aes(x = year, y = emp_level, fill=emp_level)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = round(emp_level, 1)), color="black", vjust=-0.5, position=position_dodge(0.9), size=5, fontface='bold') + 
  geom_label(x=2024,y=237.5,label='2022-2032 Projected Growth Rate: \n 35.2%', fill='white', fontface='bold', size=6) + 
  theme_fivethirtyeight() + theme(legend.position='none') + 
  theme(axis.title.y = element_text(size=16, face='bold'), 
        axis.text.x = element_text(size=14, face='bold'), axis.text.y=element_blank()) + 
  labs(title="Data Scientist Projected Employment Level", y='# Employed (Thousands)') + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  scale_x_continuous(breaks=seq(2022,2032,2)) + 
  coord_cartesian(ylim=c(150,250)) + 
  scale_fill_gradient(low='pink',high='maroon')
ggsave('charts/data_growth_time.png', width = 26, height = 16, units = "cm")


## All BLS data scientist data
all_bls <- read.csv("data/National Employment Matrix_OCC_15-2051.csv")

# detailed industry distribution of DS - top 10
all_bls_top_inds <- all_bls %>%
  ## take just the more detailed industry groupings
  filter(Display.Level==4) %>%
  ## sort by top projected emp level growth
  arrange(desc(Employment.Change..2022.2032)) %>%
  head(10)

# how many new jobs are these altogether
print(sum(all_bls_top_inds$Employment.Change..2022.2032))
print(sum(all_bls_top_inds$Employment.Change..2022.2032/all_bls$Employment.Change..2022.2032[n=1]))
# what portion of the current job market doe these industries represent
print(sum(all_bls_top_inds$X2022.Percent.of.Occupation))

ggplot(all_bls_top_inds, aes(x=reorder(Industry.Title, Employment.Change..2022.2032), y=Employment.Change..2022.2032*1000)) + 
  geom_col(fill='#AFA2FF') + coord_flip() + 
  geom_text(aes(label=ifelse(Industry.Title=="Computer systems design and related services", Industry.Title, "")), 
            color="black", size=4.5, fontface='bold', hjust=1.05) + 
  geom_text(aes(label=ifelse(Industry.Title!="Computer systems design and related services", Industry.Title, "")), 
            color="black", size=4.5, fontface='bold', hjust=-0.01) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Top 10 Industries for Data Scientist Employment Level Growth, \n 2022-2032") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 21, hjust = 0.5)) + 
  scale_y_continuous(labels=comma, expand=expansion(add=c(0,1000)))
ggsave('charts/ds_top_inds.png', width = 26, height = 16, units = "cm")

  
# Industry distribution and emp growth by industry
all_bls_clean <- all_bls %>%
  filter(Display.Level==2)

ggplot(all_bls_clean, aes(x=reorder(Industry.Title, X2022.Percent.of.Occupation), y=X2022.Percent.of.Occupation)) + 
  geom_col(fill='light blue') + coord_flip() + 
  geom_text(aes(label=ifelse(Industry.Title=="Professional, scientific, and technical services", Industry.Title, "")), 
                color="black", size=4, fontface='bold', hjust=1.1) + 
  geom_text(aes(label=ifelse(Industry.Title!="Professional, scientific, and technical services", Industry.Title, "")), 
                color="black", size=4, fontface='bold', hjust=-0.01) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Distribution of Data Scientists by Industry, 2022") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  scale_y_continuous(labels=c("0%", "10%", "20%", "30%"), expand=expansion(add=c(0,1)))
ggsave('charts/ds_industry_dist_2022.png', width = 26, height = 16, units = "cm")

# change in employment level by industry, 2022-2032
ggplot(all_bls_clean, aes(x=reorder(Industry.Title, Employment.Change..2022.2032), y=Employment.Change..2022.2032*1000)) + 
  geom_col(fill='light blue') + coord_flip() + 
  geom_text(aes(label=ifelse(Industry.Title=="Professional, scientific, and technical services", Industry.Title, "")), 
            color="black", size=4, fontface='bold', hjust=1.1) + 
  geom_text(aes(label=ifelse(Industry.Title!="Professional, scientific, and technical services", Industry.Title, "")), 
            color="black", size=4, fontface='bold', hjust=-0.01) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Employment Change of Data Scientists by Industry, 2022-2032") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  scale_y_continuous(labels=comma, expand=expansion(add=c(0,1000)))
ggsave('charts/ds_emp_change_2232.png', width = 26, height = 16, units = "cm")

# change in employment percentage by industry, 2022-2032
ggplot(all_bls_clean, aes(x=reorder(Industry.Title, Employment.Percent.Change..2022.2032), y=Employment.Percent.Change..2022.2032)) + 
  geom_col(fill='light green') + coord_flip() + 
  geom_text(aes(label=ifelse(Industry.Sort==294, Industry.Title, "")), 
            color="black", size=4, fontface='bold', hjust=0.9) + 
  geom_text(aes(label=ifelse(Industry.Sort!=294, Industry.Title, "")), 
            color="black", size=4, fontface='bold', hjust=1.1) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Employment Percent Change of Data Scientists by Industry, \n 2022-2032") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  scale_y_continuous(labels=c("0%", "10%", "20%", "30%", "40%"), expand=expansion(add=c(0,1)))
ggsave('charts/ds_emp_PERC_change_2232.png', width = 26, height = 16, units = "cm")

## BLS employment projections for detailed occupations
bls_occs <- read_excel("data/bls_emp_projection_detailed_occs.xlsx", sheet='Table 1.2', skip=1)

# compare employment growth of mathematical science occupations
bls_occs_ds_related <- bls_occs %>%
  filter(`2022 National Employment Matrix code`=="15-2011" | 
         `2022 National Employment Matrix code`=="15-2021" | 
         `2022 National Employment Matrix code`=="15-2031" |
         `2022 National Employment Matrix code`=="15-2041" |
         `2022 National Employment Matrix code`=="15-2051" |
         `2022 National Employment Matrix code`=="15-2099") %>%
  select(`2022 National Employment Matrix title`, `Employment change, numeric, 2022-32`, `Employment change, percent, 2022-32`) %>%
  mutate(`Employment change, numeric, 2022-32`=`Employment change, numeric, 2022-32`*1000) %>%
  reshape2::melt(id=c('2022 National Employment Matrix title'))

ggplot(bls_occs_ds_related, aes(x=reorder(`2022 National Employment Matrix title`, value), y=value, fill=variable)) + 
  geom_bar(stat='identity') + coord_flip() + facet_wrap(~variable, scales='free', nrow=2) + 
  geom_text(aes(label=ifelse(`2022 National Employment Matrix title`=='Data scientists', `2022 National Employment Matrix title`, "")), 
            color="black", size=4.5, fontface='bold', hjust=1.05) + 
  geom_text(aes(label=ifelse(`2022 National Employment Matrix title`!='Data scientists', `2022 National Employment Matrix title`, "")), 
            color="black", size=4.5, fontface='bold', hjust=-0.05) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Mathematical Science Occupations") + 
  theme(legend.position = 'none') + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(strip.text = element_text(face='bold', size=16)) + 
  scale_y_continuous(labels=comma, expand=expansion(add=c(0,1)))
ggsave('charts/math_science_occs_emp_change.png', width = 26, height = 16, units = "cm")

# compare data science emp growth to other occs
bls_occs2 <- read_excel("data/bls_emp_projection_detailed_occs.xlsx", sheet='Table 1.3', skip=1)

bls_occs2 <- bls_occs2 %>%
  ## sort by top projected emp level growth
  arrange(desc(`Employment change, percent, 2022-32`)) %>%
  head(10)

# employment percent change, top overall occs
ggplot(bls_occs2, aes(x=reorder(`2022 National Employment Matrix title`, `Employment change, percent, 2022-32`), y=`Employment change, percent, 2022-32`)) + 
  geom_col(fill='#CB769E') + coord_flip() + 
  geom_text(aes(label=ifelse(`2022 National Employment Matrix title`=='Data scientists', `2022 National Employment Matrix title`, "")), 
            color="white", size=4.5, fontface='bold', hjust=1.05) + 
  geom_text(aes(label=ifelse(`2022 National Employment Matrix title`!='Data scientists', `2022 National Employment Matrix title`, "")), 
            color="black", size=4.5, fontface='bold', hjust=-0.05) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Top 10 Occupations by Employment Percent Change, \n 2022-2032") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  scale_y_continuous(labels=c("0%", "20%", "40%", "60%"), expand=expansion(add=c(0,20)))
ggsave('charts/top_occs_emp_change.png', width = 26, height = 16, units = "cm")

###################################
## Google Trends (search traffic)
###################################
google_data <- read.csv("data/google_trends_fields.csv", skip=2)

# clean the google data
## replace <1 with 1 so we can plot the data as numeric
google_data[google_data=='<1'] <- '1'

google_data_clean <- google_data %>%
  dplyr::rename('Data Scientist' = data.scientist...United.States.,
         'Data Engineer' = data.engineer...United.States.,
         'Software Engineer' = software.engineer...United.States.,
         'Computer Scientist' = computer.scientist...United.States.,
         'Data Analyst' = data.analyst...United.States.) %>%
  mutate_at(c('Data Scientist', 'Data Engineer'), as.numeric) %>%
  melt(id='Month')
google_data_clean$date <- as.Date(paste(google_data_clean$Month, "-01", sep=""))

## plot major search terms
ggplot(google_data_clean, aes(x = date, y = value, group=variable, color=variable)) + 
  geom_line(linewidth=1.4) + 
  theme_fivethirtyeight() + theme(legend.position=c(0.3,0.6), legend.title = element_blank(), 
                                  legend.text = element_text(size=16), legend.direction = "vertical",
                                  legend.background = element_rect(color='black',size=1,linetype='solid'),
                                  legend.spacing.y = unit(0,'cm')) + 
  theme(axis.title.y = element_text(size=16, face='bold'), 
        axis.text.x = element_text(size=14, face='bold'), axis.text.y=element_blank()) + 
  labs(title="Google Trends Search Traffic", y='Relative Interest Index') + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand=c(0,0))  + 
  scale_color_manual(breaks=c('Software Engineer', 'Data Analyst', 'Data Scientist', 'Data Engineer', 'Computer Scientist'),
    values=c("#226F54", "#74226C", "#2892D7", "#F06449", "#8FC93A"))
ggsave('charts/google_search_fields.png', width = 26, height = 16, units = "cm")


## plot data science sub search terms
google_subtrends <- read.csv("data/google_subtrends.csv", skip=2)

## replace <1 with 1 so we can plot the data as numeric
google_subtrends[google_subtrends=='<1'] <- '1'

google_subtrends_clean <- google_subtrends %>%
  rename('Data Science Masters' = data.science.masters...United.States.,
         'Data Science Jobs' = data.science.jobs...United.States.,
         'Data Science Salary' = data.science.salary...United.States.) %>%
  mutate_at(c('Data Science Masters', 'Data Science Jobs', 'Data Science Salary'), as.numeric) %>%
  melt(id='Month')
google_subtrends_clean$date <- as.Date(paste(google_subtrends_clean$Month, "-01", sep=""))

ggplot(google_subtrends_clean, aes(x = date, y = value, group=variable, color=variable)) + 
  geom_line(size=1.4) + 
  theme_fivethirtyeight() + theme(legend.position=c(0.3,0.6), legend.title = element_blank(), 
                                  legend.text = element_text(size=16), legend.direction = "vertical",
                                  legend.background = element_rect(color='black',size=1,linetype='solid'),
                                  legend.spacing.y = unit(0,'cm')) + 
  theme(axis.title.y = element_text(size=16, face='bold'), 
        axis.text.x = element_text(size=14, face='bold'), axis.text.y=element_blank()) + 
  labs(title="Google Trends Search Traffic", y='Relative Interest Index') + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand=c(0,0))  + 
  scale_color_manual(values=c("maroon", "#BCD979","navy"))
ggsave('charts/google_search_ds_terms.png', width = 26, height = 16, units = "cm")


###################################
## BLS OES Wage Statistics
###################################
## State map of DS salaries
ds_wages_states <- read_excel("data/bls_oes_ds_median_wage_states.xlsx", skip=5)

# clean data
ds_wages_states$`Annual median wage(2)` <- as.numeric(ds_wages_states$`Annual median wage(2)`)
ds_wages_states$state <- sub("\\s*\\(.*", "", ds_wages_states$`Area Name`)

plot_usmap(data = ds_wages_states, values = "Annual median wage(2)") +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value="white", 
                       limits=c(50000, 150000), 
                       labels=c("$50,000", "$75,000","$100,000", "$125,000", "$150,000")) + 
  labs(title = "Annual Median Wage of Data Scientists, May 2022") + 
  theme_classic() + theme(axis.line = element_blank()) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  theme(legend.position='bottom', legend.title = element_blank(), 
        legend.text = element_text(size=14), legend.direction = "horizontal",
        legend.background = element_rect(color='black',size=1,linetype='solid',), 
        legend.key.width = unit(2, "cm"), legend.margin = margin(10, 50, 10, 50)) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic")) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
ggsave('charts/ds_wages_state_map.png', width = 26, height = 16, units = "cm")

## Comparison of DS wages by industry
ds_wages_inds <- read_excel("data/bls_oes_median_wage_industries.xlsx")

ggplot(ds_wages_inds, aes(x=reorder(occupation, annual_median_wage), y=annual_median_wage)) + 
  geom_col(fill='#98C9A3') + coord_flip() + 
  geom_text(aes(label=ifelse(occupation=="Administrative and Support and Waste Management and Remediation Services", occupation, "")), 
            color="black", size=4, fontface='bold', hjust=0.8) + 
  geom_text(aes(label=ifelse(occupation!="Administrative and Support and Waste Management and Remediation Services", occupation, "")), 
            color="black", size=4, fontface='bold', hjust=1.05) + 
  theme_fivethirtyeight() + theme(axis.text.x = element_text(size=14), axis.text.y=element_blank()) + 
  labs(title="Annual Median Wages of Data Scientists by Industry, \n May 2022") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid")) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) + 
  scale_y_continuous(labels=dollar, expand=expansion(add=c(0,10000)))
ggsave('charts/ds_wages_industries.png', width = 26, height = 16, units = "cm")
