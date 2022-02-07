// immigration_trends.do
// Imports the IPUMS USA data and cleans the data for mapping in R. 
// Requires: ipums_cleaned.dta
// Last edited 2/7/22, by Tal Roded
********************************************************************************
clear all

*directory path
global path "C:\Users\C1TGR01\Desktop\VisualizeCuriosity\immigration_trends\data"

use "$path\ipums_cleaned", clear

**Cleaning and creating variables needed for later calculations
*counter of observations (unit of observation = individuals)
gen count=1
*egen id=group(serial pernum)

tempfile cleaned
save `cleaned'
*****************************************
** Descriptive statistics of immigrant population/natives
*****************************************
**Chart #1: time series of immigration by birthplace
*keep immigrants who arrived 2006 or later
drop if bpl<=120
drop if yrimmig<2006

collapse (sum) count [fweight=perwt], by(bpl yrimmig) 
sort count
bysort yrimmig: egen total=total(count)
gen prop=100*count/total
*keep top 10 birthplaces of immigrants in 2019 for cleaner chart
bysort yrimmig: egen rank=rank(prop), field
bysort bpl (yrimmig): gen rank2019=rank[_N]
drop if rank2019>10
*clean yrimmig variable for easier charting
tostring yrimmig, force replace
export excel "$path\ipums_data_for_mapping.xlsx", sheet("by_bpl") firstrow(var) sheetreplace
*what % of total immigration did the top 10 count for in each year?
bysort yrimmig: egen prop_total=total(prop)

**Age distributions
use `cleaned', clear
gen native=(bpl<=120)
*again, just want to focus on immigrants for 2006-2019 period
drop if native==0 & yrimmig<2006
collapse (sum) count [fweight=perwt], by(native age)
tostring native, replace
replace native="Immigrants" if native=="0"
replace native="Natives" if native=="1"

*descriptive statistics to discuss in the post
collapse (mean) age (median) medage=age [fweight=count], by(native)

**Gender comparison
use `cleaned', clear
gen native=(bpl<=120)
*again, just want to focus on immigrants for 2006-2019 period
drop if native==0 & yrimmig<2006
collapse (sum) count [fweight=perwt], by(native sex)
bysort native: egen total=total(count)
gen prop=100*count/total

**Charts #2 and #3: edu and income distributions of immigrants and natives
**Education breakdowns (bar chart)
use `cleaned', clear
gen native=(bpl<=120)
*again, just want to focus on immigrants for 2006-2019 period
drop if native==0 & yrimmig<2006
*drop if no schooling information
drop if educd==999
*drop if below age 25
drop if age<25
*place educ variable in my desired categories
recode educd (000/002=0) (010/061=1) (062/064=2) (065/100=3) (101=4) (110/116=5)
collapse (sum) count [fweight=perwt], by(native educd)
*provide value labels
tostring educd, force replace
destring educd, replace
label define educlabel 0 "No schooling" 1 "Less than high school" 2 "High School" 3 "Some college" 4 "Bachelor's degree" 5 "Graduate degree"
label values educd educlabel
bysort native: egen total=total(count)
gen prop=100*count/total
*clean and export
tostring native, replace
replace native="Immigrants" if native=="0"
replace native="Natives" if native=="1"
export excel "$path\ipums_data_for_mapping.xlsx", sheet("educ_dists") firstrow(var) sheetreplace

**Income distributions
use `cleaned', clear
gen native=(bpl<=120)
*again, just want to focus on immigrants for 2006-2019 period
drop if native==0 & yrimmig<2006
*median income of 16+ population, for those with positive earnings
drop if age<16
drop if inctot<=1 | inctot>=9999999
*adjust income for inflation
*replace with log income
gen loginc = log(inctot)

*distribution of incomes for natives and immigrants
twoway (histogram inctot if native==1, color(red%30) bin(50)) ///        
       (histogram inctot if native==0, color(green%30) bin(50)), ///   
       legend(order(1 "Natives" 2 "Immigrants" )) 
twoway (histogram loginc if native==1, color(red%30) bin(50)) ///        
       (histogram loginc if native==0, color(green%30) bin(50)), ///   
       legend(order(1 "Natives" 2 "Immigrants" )) 
tab native, summ(inctot)
collapse (median) inctot [fw=perwt], by(native)

**More statistics on labor force
use `cleaned', clear
*median income of 16+ population, for those with positive earnings
drop if age<16
gen native=(bpl<=120)
drop if native==0 & yrimmig<2006
*collapse (median) inctot [fw=perwt], by(native)
*percent of 16+ natives and immigrants in labor force categories
drop if empstat==0
gen emp=(empstat==1)
gen unemp=(empstat==2)
gen nilf=(empstat==3)
collapse (sum) count emp unemp nilf [fw=perwt], by(native)
gen prop_lf = 100*(1-(nilf/count))
gen prop_emp = 100*(emp/count)
gen prop_unemp = 100*(unemp/count)


*****************************************
** Summary statistics of where immigrants are located
*****************************************
use "$path\ipums_cleaned", clear

*keep immigrants who arrived 2006 or later
drop if bpl<=120
drop if yrimmig<2006

gen count=1

*cleaning so it's easier to match with my fips codes dataset later
rename (statefip countyfip) (state_fips county_fips)
tostring state_fips, force replace
destring state_fips, replace

tempfile immigrants
save `immigrants'

collapse (sum) count [fw=perwt], by(state_fips county_fips bpl)

by state_fips county_fips: egen total=total(count)
gen prop=100*count/total

export excel "$path\ipums_data_for_mapping.xlsx", sheet("all_locations") firstrow(var) sheetreplace

*top locations of immigrants overall
use `immigrants', clear

collapse (sum) count [fw=perwt], by(state_fips county_fips)

egen total=total(count)
gen prop=100*count/total

*what portion of a state's immigrants are in each of that state's counties?
bys state_fips: egen state_total = total(count)
gen county_prop=100*count/state_total
sort county_prop


*location changes of immigrants
use "$path\ipums_cleaned", clear

*keep immigrants and natives, compare locations in 2006 and 2018
gen native=1 if bpl<=120
gen immigrant=1 if native==.
keep if year==2006 | year==2018

gen count=1

*cleaning so it's easier to match with my fips codes dataset later
rename (statefip countyfip) (state_fips county_fips)
tostring state_fips, force replace
destring state_fips, replace
collapse (sum) native immigrant count [fw=perwt], by(state_fips county_fips year)

*proportion immigrant comparisons for each county
gen prop_imm=100*(immigrant/count)
bysort state_fips county_fips (year): gen imm_change=prop_imm[2] - prop_imm[1] 
bysort state_fips county_fips (year): gen imm_change_prop=100*(prop_imm[2] - prop_imm[1])/prop_imm[1]

*only need one obs per county
keep if year==2018
drop if imm_change==.
keep state_fips county_fips prop_imm imm_change imm_change_prop
export excel "$path\ipums_data_for_mapping.xlsx", sheet("location_changes") firstrow(var) sheetreplace
