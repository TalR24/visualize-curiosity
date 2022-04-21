**atus_analyze.do
**Using ATUS data imported from IPUMS, compare trends in time use by income levels. For "K-Shaped Recovery in Time" VizCur post
**Requires: cleaned_ipums_atus.dta
**Last edited: 4/20/22 by Tal Roded
************************************************************************
clear all

global proj_path "C:\Users\C1TGR01\Desktop\VisualizeCuriosity\K_Shaped_Recovery"

use "$proj_path\data\cleaned_ipums_atus.dta", clear

************************************************
**Additional data cleaning and variable creation
************************************************
drop pernum lineno

*replacing not in universe responses with blanks
replace earnweek=. if earnweek==99999.99
replace hourwage=. if hourwage==999.99
replace covidtelew=. if covidtelew==99
replace covidunaw=. if covidunaw==99

gen yearly_income=earnweek*52
drop if yearly_income==.
gen income_tiers="low" if yearly_income<30000
replace income_tiers="high" if yearly_income>120000

*note: specifying pweights as aweights produces identical point estimates
tab income_tiers [aw=wt20], missing

************************************************
**Compare characteristics of low- and high-income respondents
************************************************
tab sex income_tiers [aw=wt20], column
tab income_tiers [aw=wt20], summ(age)
tab race income_tiers [aw=wt20], column
tab clwkr income_tiers [aw=wt20], column
tab fullpart income_tiers [aw=wt20], column

*covid-related summary statistics
tab covidtelew income_tiers [aw=wt20], column
tab covidunaw income_tiers [aw=wt20], column

drop if income_tiers==""
gen count=1

save "$proj_path\data\cleaned_atus.dta", replace


**Charts comparing income tiers on demographics
*chart for educational attainment by income tier
recode educ (10/21 = 1) (30/32 = 2) (40 = 3) (41/43 = 4)
collapse (sum) count [aw=wt20], by(educ income_tiers)
bys income_tiers: egen total=total(count)
gen prop=100*(count/total)

export excel "$proj_path\data\atus_data_for_charting.xlsx", sheet("educ_income_tiers") firstrow(var) sheetreplace

*chart for major occupation type by income tier
use "$proj_path\data\cleaned_atus.dta", clear

tab occ2

decode occ2, generate(occ2s)
replace occ2s=substr(occ2s, 1, strlen(occ2s) - 12)

collapse (sum) count [aw=wt20], by(occ2s income_tiers)
bys income_tiers: egen total=total(count)
gen prop=100*(count/total)

export excel "$proj_path\data\atus_data_for_charting.xlsx", sheet("occs_income_tiers") firstrow(var) sheetreplace

************************************************
**Charts comparing income tiers on time usage
************************************************
**Averages across entire sample
use "$proj_path\data\cleaned_atus.dta", clear

*we want just one observation for each person when using activities list
duplicates drop caseid, force

collapse (mean) act* [aw=wt20], by(income_tiers year)

*drop activities that have very small durations
drop act_educ act_hhserv act_profserv act_vol 
drop activity

local actlist act_food act_hhact act_pcare act_purch act_social act_sports act_travel act_work

*what portion of the 24 hour (1440 minute) day do these activities add up to?
egen total_dur = rowtotal(`actlist')
gen portionofday = 100*total_dur/1440

reshape long act_, i(year income_tiers) j(activity) string
rename act_ duration

replace duration = duration/60

export excel "$proj_path\data\atus_data_for_charting.xlsx", sheet("act_comps_tiers_years") firstrow(var) sheetreplace


**Averages among those who did the activity (positive selection)
use "$proj_path\data\cleaned_atus.dta", clear

*we want just one observation for each person when using activities list
duplicates drop caseid, force

*drop activities that have very small durations
drop act_educ act_hhserv act_profserv act_vol 

local actlist act_food act_hhact act_pcare act_purch act_social act_sports act_travel act_work

foreach var of local actlist {
	replace `var'=. if `var'==0
}
collapse (mean) `actlist' [aw=wt20], by(income_tiers year)

*what portion of the 24 hour (1440 minute) day do these activities add up to?
egen total_dur = rowtotal(`actlist')
gen portionofday = 100*total_dur/1440

reshape long act_, i(year income_tiers) j(activity) string
rename act_ duration

replace duration = duration/60

export excel "$proj_path\data\atus_data_for_charting.xlsx", sheet("act_comps_tiers_yearsN") firstrow(var) sheetreplace


************************************************
**Charts comparing time usage by location
************************************************
use "$proj_path\data\cleaned_atus.dta", clear

collapse (mean) duration [aw=wt20], by(income_tiers where year)

*what portion of the 24 hour (1440 minute) day do these activities add up to?
bys year income_tiers: egen total_dur = total(duration)
gen portionofday = 100*total_dur/1440

decode where, generate(location)
drop if location=="NIU (Not in universe)" | location=="Other mode of transportation" | location=="Refused" | location=="Boat/ferry" | location=="Unspecified place" | location=="Other place" | location=="Post Office" | location=="Other store/mall" | location=="Place of worship" | location=="Grocery store" | location=="Walking" | location=="Bank"

replace location="Motor vehicle driver" if location=="Driver of car, truck, or motorcycle"
replace location="Motor vehicle passenger" if location=="Passenger of car, truck, or motorcycle"
replace location="Home or yard" if location=="R's home or yard"
replace location="Workplace" if location=="R's workplace"
replace location="Taxi/limousine" if location=="Taxi/limousine service"
replace location="Outdoors" if location=="Outdoors--not at home"

replace duration = duration/60

export excel "$proj_path\data\atus_data_for_charting.xlsx", sheet("act_locations") firstrow(var) sheetreplace

