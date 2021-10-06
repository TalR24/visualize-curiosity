# studentloans_post.R
# Takes excel data transformed from STATA and creates various charts to visualize data trends.
# Created by Tal Roded on 8/28/18, last edited on 8/28/18
#############################################################################################################################################
# All necessary libraries
library(readxl)
library(ggplot2)
library(ggmap)
library(dplyr)
library(RColorBrewer)
library(ggthemes)
library(scales)
library(extrafont)
library(maps)
font_import()

windowsFonts(font1 = windowsFont("Gill Sans"))
windowsFonts(font2 = windowsFont("Calibri"))
windowsFonts(font3 = windowsFont("Franklin Gothic Book"))
windowsFonts(font4 = windowsFont("Garamond"))


# pie charts of loan types
loan_totals <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_totals_modified.xlsx")
total_df2007 <- data.frame(group = c("Perkins Loans", "Direct Loans", "FFE Loans"), value = c(1.6, 20.7, 77.9))
total_bar2007 <- ggplot(total_df2007, aes(x="", y=value, fill=reorder(group, -value))) + geom_bar(width=1, stat = "identity")
total_pie2007 <- total_bar2007 + coord_polar("y")
total_pie2007 + scale_fill_manual(values = c("#2F4858","#33658A","#86BBD8")) + theme_minimal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), 
                                                                                                       panel.border=element_blank(), panel.grid=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) + 
  geom_text(position = position_stack(vjust=0.5), aes(x = 1.64, label = percent(value/100)), size=6, family = "font3") +
  labs(fill = "Loan Type", title = "Breakdown of Federal Student Loans, 2007") + theme(plot.title = element_text(hjust=0.6, vjust = -1, size = 24, face = "bold", family = "font4")) + 
  theme(legend.title = element_text(face = "bold", family = "font1")) + theme(legend.position = c(1, 0.5))



total_df2018 <- data.frame(group = c("Perkins Loans", "FFE Loans", "Direct Loans"), value = c(0.5, 21.0, 78.5))
total_bar2018 <- ggplot(total_df2018, aes(x="", y=value, fill=reorder(group, -value))) + geom_bar(width=1, stat = "identity")
total_pie2018 <- total_bar2018 + coord_polar("y")
total_pie2018 + scale_fill_manual(values = c("#33658A", "#2F4858", "#86BBD8")) + theme_minimal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), 
                                                                                                         panel.border=element_blank(), panel.grid=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) + 
  geom_text(position = position_stack(vjust=0.5), aes(x = 1.64, label = percent(value/100)), size=6, family = "font3") +
  labs(fill = "Loan Type", title = "Breakdown of Federal Student Loans, 2018") + theme(plot.title = element_text(hjust=0.6, vjust = -1, size = 24, face = "bold", family = "font4")) + 
  theme(legend.title = element_text(face = "bold", family = "font1")) + theme(legend.position = c(1, 0.5))




# bar + line chart of loan balances
loan_balance <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_totals_balance.xlsx")
total_balance <- ggplot(data = loan_balance) + geom_bar(aes(x=FederalFiscalYear, y=TotalDollarsOutstandingin), stat="identity", fill = "#e5d4c0") + 
  geom_line(aes(x=FederalFiscalYear,y=totalgr*max(loan_balance$TotalDollarsOutstandingin), color = "totalgr"), size = 1.25, alpha = 0.8) + 
  geom_line(aes(x=FederalFiscalYear,y=yearlygr*max(loan_balance$TotalDollarsOutstandingin), color = "yearlygr"), size = 1.25) + 
  geom_text(aes(label=sprintf("%1.0f%%",100*round(totalgr, digits = 3)),x=FederalFiscalYear,y=totalgr*max(loan_balance$TotalDollarsOutstandingin)), fontface = "bold", color = "#134074") + 
  geom_text(aes(label=sprintf("%1.0f%%",100*round(yearlygr, digits = 3)),x=FederalFiscalYear,y=yearlygr*max(loan_balance$TotalDollarsOutstandingin)), fontface = "bold", color = "#134611") +
  geom_text(aes(label=TotalDollarsOutstandingin, x=FederalFiscalYear, y=TotalDollarsOutstandingin-50), color = "#454545", fontface = "bold")
total_balance + theme_bw() + scale_x_continuous(breaks=c(2008,2010,2012,2014,2016,2018)) + 
  labs(y = "Total $ Balance (in billions)", title = "Growth of Federal Student Loans", color = "Growth Rates") + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = "bold", family = "font4")) + 
  theme(panel.grid.minor = element_blank()) + 
  scale_color_manual(labels = c("Cumulative", "Yearly"), values = c("#90bede", "#bad4aa")) + 
  theme(legend.position = "top") + theme(legend.title = element_text(size = 10)) + theme(axis.text.y.left = element_blank()) + 
  theme(axis.ticks.y.left= element_blank()) + theme(axis.title.y.left = element_text(size = 16, family = "font4")) + 
  theme(axis.title.x.bottom = element_blank())


# bar + line chart of loan recipients
loan_recipient <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_totals_recipient.xlsx")
total_recipient <- ggplot(data = loan_recipient) + geom_bar(aes(x=FederalFiscalYear, y=Recipients), stat="identity", fill = "#cea0ae", alpha = 0.925) + 
  geom_line(aes(x=FederalFiscalYear,y=totalgr_rec*max(loan_recipient$Recipients), color = "totalgr"), size = 1.25, alpha = 0.8) + 
  geom_line(aes(x=FederalFiscalYear,y=yearlygr_rec*max(loan_recipient$Recipients), color = "yearlygr"), size = 1.25) + 
  geom_text(aes(label=sprintf("%1.0f%%",100*round(totalgr_rec, digits = 3)),x=FederalFiscalYear,y=totalgr_rec*max(loan_recipient$Recipients)), fontface = "bold", color = "#134074") + 
  geom_text(aes(label=sprintf("%1.0f%%",100*round(yearlygr_rec, digits = 3)),x=FederalFiscalYear,y=yearlygr_rec*max(loan_recipient$Recipients)), fontface = "bold", color = "#134611") +
  geom_text(aes(label=Recipients, x=FederalFiscalYear, y=Recipients-1), color = "#210b2c", fontface = "bold")
total_recipient + theme_bw() + scale_x_continuous(breaks=c(2008,2010,2012,2014,2016,2018)) + 
  labs(y = "Total Borrowers (in millions)", title = "Growth of Federal Student Loan Borrowers", color = "Growth Rates") + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = "bold", family = "font4")) + 
  theme(panel.grid.minor = element_blank()) + 
  scale_color_manual(labels = c("Cumulative", "Yearly"), values = c("#90bede", "#bad4aa")) + 
  theme(legend.position = "top") + theme(legend.title = element_text(size = 10)) + theme(axis.text.y.left = element_blank()) + 
  theme(axis.ticks.y.left= element_blank()) + theme(axis.title.y.left = element_text(size = 16, family = "font4")) + 
  theme(axis.title.x.bottom = element_blank())




# loan locations data
loan_locations <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_locations_modified.xlsx")
all_states <- map_data('state')
ggplot(data = all_states) + geom_polygon(aes(x = long, y = lat, group = group), color = "white") + coord_map() + theme_void()
locations_map <- inner_join(all_states, loan_locations, by = "region")

ggplot(data = locations_map) + coord_map() + geom_polygon(aes(x = long, y = lat, group = region, fill = diff_avg), color = "white") +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, limits = c(-6000, 6000), labels = c("-$6,000", "-$3,000", "$0", "+$3,000", "+$6,000")) + labs(title = "Average Student Loan Balance", subtitle = "Difference from National Average") + 
  theme_classic() + theme(axis.line = element_blank()) + theme(legend.text = element_text(vjust = 0.2)) + 
  theme(axis.title = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank()) + 
  theme(plot.title = element_text(face = "bold", family = "font2", size = 22, hjust = 0.5)) + theme(legend.position = "bottom") + 
  theme(legend.key.width=unit(2,"cm")) + theme(legend.justification = 0.35) + theme(legend.title = element_blank()) + 
  theme(plot.subtitle = element_text(family = "font2", hjust = 0.5, size = 14))




# loan school types data
loan_schools <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_schooltype_modified.xlsx")
ggplot(data = loan_schools, aes(x=type, y=portion)) + geom_bar(aes(fill=reorder(category, -portion)), stat="identity", position = "dodge") + 
  geom_text(aes(label=sprintf("%1.0f%%",portion), x=c(0.71,1.01,1.31,1.71,2.01,2.31), y=portion+1.5), fontface = "bold", size = 6) + 
  theme_fivethirtyeight() + scale_x_discrete(labels = c("Debt", "Borrowers")) + labs(title = "Federal Student Loans by School") + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = "bold", family = "font4")) + theme(panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = c("#2978a0", "#034732", "#f06449")) + theme(legend.title = element_text(size = 10)) + 
  theme(axis.title.y.left = element_blank()) + theme(axis.title.x.bottom = element_blank()) + theme(axis.text.y.left = element_blank()) + 
  theme(axis.ticks.y.left = element_blank()) + theme(legend.title = element_blank()) + theme(legend.text = element_text(vjust = 0.5, family = "font3", size = 12))




loan_avg <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_schooltype_avg.xlsx")
ggplot(data = loan_avg, aes(x=reorder(category,this), y=amount)) + geom_bar(aes(fill=reorder(category, this)), stat="identity", position = "dodge") + 
  geom_text(aes(label=c("$24,487", "$35,052", "$20,032"), x=c(1,2,3), y=amount+1000), fontface = "bold", size = 6) + 
  theme_fivethirtyeight() + labs(title = "Average Federal Student Loan Balance by School") + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = "bold", family = "font4")) + theme(panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = c("#2978a0", "#034732", "#f06449")) + theme(axis.title.y.left = element_blank()) + 
  theme(axis.title.x.bottom = element_blank()) + theme(axis.text.y.left = element_blank()) + theme(axis.ticks.y.left = element_blank()) + 
  theme(legend.position = "none") + theme(axis.text.x.bottom = element_text(family = "font3", size = 12))




# loan delinquencies data
loan_bal <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_delinquencies_bal.xlsx")
ggplot(data = loan_bal) + geom_line(aes(x=year, y=amount, color = type), size = 1.25) + theme_economist_white() + 
  labs(title = "Loan Delinquency Rates", y = "Outstanding Balance (in billions)") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = c(0.225, 0.885)) + 
  scale_color_brewer(palette = "Set1", labels = c("181-270 Days Delinquent", "271 - 360 Days Delinquent", "31-90 Days Delinquent", "91-180 Days Delinquent", "Transferring to Debt Collection")) + 
  theme(axis.title.x = element_blank()) + theme(plot.title = element_text(family = "font4", size = 20)) + 
  theme(axis.title.y = element_text(family = "font3", size = 14, vjust = 2, hjust = 0.35)) + 
  scale_y_continuous(labels = c("$0", "$10", "$20", "$30", "$40"))


loan_bor <- read_excel("C:/Users/C1TGR01/Desktop/Blog Post Components/StudentLoans_Post/loan_delinquencies_bor.xlsx")
ggplot(data = loan_bor) + geom_line(aes(x=year, y=amount, color = type), size = 1.25) + theme_economist_white() + 
  labs(title = "Loan Delinquency Rates, Borrowers", y = "# of Borrowers (in millions)") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title = element_blank()) + theme(legend.position = "right") + 
  scale_color_brewer(palette = "Set1", labels = c("181-270 Days Delinquent", "271 - 360 Days Delinquent", "31-90 Days Delinquent", "91-180 Days Delinquent", "Transferring to Debt Collection")) + 
  theme(axis.title.x = element_blank()) + theme(plot.title = element_text(family = "font4", size = 20)) + 
  theme(axis.title.y = element_text(family = "font3", size = 14, vjust = 2, hjust = 0.35))
