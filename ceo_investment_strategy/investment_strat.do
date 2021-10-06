********************************************************************************
** investment_strat.do
** This program is to input the historical stock performance data taken from Yahoo Finance,
** merge the data, clean it, and prepare for visualization in R.
** Created by Tal Roded on 8/14/18
********************************************************************************
clear all

cd "C:\Users\C1TGR01\Desktop\Blog Post Components\InvestmentStrat_Post"

// import S&P 500 Index
import delimited using "GSPC.csv", clear
gen name = "Z_index"
keep name date close
order name date close
save GSPC, replace

// import all stocks
foreach var in AAPL BA BAC CAT CMCSA CSCO GOOG GS IBM INTC JPM MCD MMM MSFT PEP V {
import delimited using "`var'.csv", clear
	keep stock stock_close date
	rename stock name
	rename stock_close close
	order name date close
	// add in index data
	append using "GSPC.dta", keep(name date close)
	duplicates tag date, gen(dup)
	keep if dup==1
	drop dup
	sort date name
	// generate index
	gen index = (close[_n]/close[1]) if name=="`var'"
	replace index = (close[_n]/close[2]) if name=="Z_index"
	order name close index date
	save "`var'", replace
	export delimited using "`var'1", replace
	}

// forgot amazon
import delimited using "AMZN.csv", clear
gen stock = "AMZN"
rename close stock_close
keep stock stock_close date
rename stock name
rename stock_close close
order name date close
append using "GSPC.dta", keep(name date close)
duplicates tag date, gen(dup)
keep if dup==1
drop dup
sort date name
gen index = (close[_n]/close[1]) if name=="AMZN"
replace index = (close[_n]/close[2]) if name=="Z_index"
order name close index date
save "AMZN", replace
export delimited using "AMZN1", replace


// All stocks
use AAPL.dta, clear
foreach var in AMZN BA BAC CAT CMCSA CSCO GOOG GS INTC JPM MCD MMM MSFT PEP V {
	append using "`var'"
	drop if name=="Z_index"
	}
append using "IBM"
sort date name
replace name="S&P 500 Index" if name=="Z_index"
save "allstocks", replace
export delimited using "allstocks", replace

// Tech stocks
use AAPL.dta, clear
foreach var in AMZN CMCSA CSCO GOOG INTC MSFT {
	append using "`var'"
	drop if name=="Z_index"
	}
append using "IBM"
sort date name
replace name="S&P 500 Index" if name=="Z_index"
save "techstocks", replace
export delimited using "techstocks", replace



// Industrial stocks
use BA.dta, clear
foreach var in MMM {
	append using "`var'"
	drop if name=="Z_index"
	}
append using "CAT"
sort date name
replace name="S&P 500 Index" if name=="Z_index"
save "industrialstocks", replace
export delimited using "industrialstocks", replace


// Financial stocks
use BAC.dta, clear
foreach var in GS V {
	append using "`var'"
	drop if name=="Z_index"
	}
append using "JPM"
sort date name
replace name="S&P 500 Index" if name=="Z_index"
save "financialstocks", replace
export delimited using "financialstocks", replace


// Food stocks
use PEP.dta, clear
drop if name=="Z_index"
append using "MCD"
sort date name
replace name="S&P 500 Index" if name=="Z_index"
save "foodstocks", replace
export delimited using "foodstocks", replace
