*** DDD ANALYSIS

use ${raw_data}/data_gtrends.dta, clear

set more off

so geo date

ta  geo, gen(geo)
forv k = 2004/2020{
	gen y`k' = (year ==`k')
}
egen group = group(geo)
egen pop_mean = mean(pop) if month == 1 , by(group)
bysort group: carryforward pop_mean, replace

*** Generate state-specific trends (linear, then quadratic)
so geo date
by geo: gen trend = _n
forv k = 1/51{
gen tl_geo`k' = trend*geo`k'
}
forv k = 1/51{
gen tq_geo`k' = trend*trend*geo`k'
}

replace under_lockdown = 0 if under_lockdown ==.

gen aft_cases_underlock = after_cases*under_lockdown
gen confirmed_pop_reg = confirmed_pop / 1000
gen deaths_pop_reg = deaths_pop / 1000


label variable under_lockdown "Lockdown"
label variable after_cases "Pandemic Threshold"
label variable aft_cases_underlock "Lockdown * Pandemic Threshold"
label variable confirmed_pop_reg "Cases (pop.)"
label variable deaths_pop_reg "Deaths (pop.)"


****************************
***     DDD analysis     ***
***  Main specification  ***
****************************

set matsize 6000

global spe_DD_cases "after_cases ib(4).month i.year i.group" 
global spe_DD_lockdown "under_lockdown ib(4).month i.year i.group" 
global spe_DDD "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group" 

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_DD_lockdown if has_lockdown == 1 [aw=pop_mean] , cluster(geo)
	est store DD_lock
	
	replace after_cases = 0
	replace after_cases = 1 if confirmed > 10000
	bysort geo: egen has_cases = max(after_cases)
	replace aft_cases_underlock = after_cases*under_lockdown
	xi : reg `v' $spe_DD_cases if has_cases == 1 [aw=pop_mean] , cluster(geo)
	est store DD_cases
	
	xi : reg `v' $spe_DDD [aw=pop_mean] , cluster(geo)
	est store DDD
	
	drop has_cases
	
	replace after_cases = 0
	replace after_cases = 1 if confirmed_pop > 3000
	bysort geo: egen has_cases = max(after_cases)
	replace aft_cases_underlock = after_cases*under_lockdown
	xi : reg `v' $spe_DD_cases if has_cases == 1 [aw=pop_mean] , cluster(geo)
	est store DD_cases_pop
	
	xi : reg `v' $spe_DDD [aw=pop_mean] , cluster(geo)
	est store DDD_pop
		
	drop has_cases

	restore
	
	
	esttab DD_lock DD_cases DD_cases_pop DDD DDD_pop using ${DDD}/Regression/DDD.tex, drop(*year* *group* *month*) se obslast nodepvars ///
	title("Main - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Lockdown DD" "Pandemic DD (abs.)" "Pandemic DD (pop.)" "DDD (abs.)" "DDD (pop.)") page append
	
}

global spe_DDD_lin "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_DDD_lin_quad "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_DDD_conf "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group confirmed_pop_reg" 
global spe_DDD_deaths "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group deaths_pop_reg" 


****************************
***     DDD analysis     ***
***  Robusteness (abs.)  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed > 10000
bysort geo: egen has_cases = max(after_cases)

set matsize 6000

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_DDD [aw=pop_mean] , cluster(geo)
	est store DDD_main
	
	xi : reg `v' $spe_DDD  , cluster(geo)
	est store DDD_nopop
	
	xi : reg `v' $spe_DDD_lin  [aw=pop_mean], cluster(geo)
	est store DDD_lin
	
	xi : reg `v' $spe_DDD_lin_quad [aw=pop_mean], cluster(geo)
	est store DDD_lin_quad
	
	xi : reg `v' $spe_DDD_conf  [aw=pop_mean], cluster(geo)
	est store DDD_conf
	
	xi : reg `v' $spe_DDD_deaths  [aw=pop_mean], cluster(geo)
	est store DDD_deaths
	
	restore
	
	xi : reg `v' $spe_DDD [aw=pop_mean] , cluster(geo)
	est store DDD_2004
	
	
	esttab DDD_main DDD_nopop DDD_lin DDD_lin_quad DDD_conf DDD_deaths DDD_2004 using ${DDD}/Regression/Rob_DDD.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Robustness (abs.) - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("DDD" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases

****************************
***     DDD analysis     ***
***  Robusteness (pop.)  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed_pop > 3000
bysort geo: egen has_cases = max(after_cases)

set matsize 6000

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_DDD  [aw=pop_mean], cluster(geo)
	est store DDD_main
	
	xi : reg `v' $spe_DDD  , cluster(geo)
	est store DDD_nopop
	
	xi : reg `v' $spe_DDD_lin [aw=pop_mean], cluster(geo)
	est store DDD_lin
	
	xi : reg `v' $spe_DDD_lin_quad  [aw=pop_mean], cluster(geo)
	est store DDD_lin_quad
	
	xi : reg `v' $spe_DDD_conf [aw=pop_mean], cluster(geo)
	est store DDD_conf
	
	xi : reg `v' $spe_DDD_deaths  [aw=pop_mean], cluster(geo)
	est store DDD_deaths
	
	restore
	
	xi : reg `v' $spe_DDD [aw=pop_mean], cluster(geo)
	est store DDD_2004
	
	esttab DDD_main DDD_nopop DDD_lin DDD_lin_quad DDD_conf DDD_deaths DDD_2004 using ${DDD}/Regression/Rob_pop_DDD.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Robustness (pop.) - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("DDD" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases


****************************
***     DDD analysis     ***
***   Placebo Lockdown   ***
****************************

drop if year < 2009

gen pl_under_lockdown = 0
by geo: replace pl_under_lockdown = under_lockdown[_n+12]
label variable pl_under_lockdown "Placebo Lockdown"
gen pl_confirmed = 0
by geo: replace pl_confirmed = confirmed[_n+12]
gen pl_confirmed_pop = 0
by geo: replace pl_confirmed_pop = confirmed_pop[_n+12]
gen pl_after_cases = 0
label variable pl_after_cases "Placebo Pandemic Threshold"
gen pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown
label variable pl_aft_cases_underlock "Placebo Lockdown * Placebo Pandemic Threshold"


set matsize 6000

global spe_pl "pl_under_lockdown pl_after_cases pl_aft_cases_underlock ib(4).month i.year i.group" 


preserve
drop if year > 2019
drop if year == 2019 & month > 10

qui foreach v of varlist div_ref - div_long_ref {
	
	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed > 10000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl [aw=pop_mean], cluster(geo)
	est store pl19_`v'
	
	drop pl_has_cases
	
	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed_pop > 3000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl [aw=pop_mean], cluster(geo)
	est store plp19_`v'
	
	drop pl_has_cases
		
}

restore
	
by geo: replace pl_under_lockdown = under_lockdown[_n+24]
by geo: replace pl_confirmed = confirmed[_n+24]
by geo: replace pl_confirmed_pop = confirmed_pop[_n+24]
	
preserve
drop if year > 2018
drop if year == 2018 & month > 10
	
qui foreach v of varlist div_ref - div_long_ref {

	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed > 10000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl [aw=pop_mean], cluster(geo)
	est store pl18_`v'
	
	drop pl_has_cases
	
	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed_pop > 3000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl [aw=pop_mean], cluster(geo)
	est store plp18_`v'
	
	drop pl_has_cases

}	
restore

qui foreach v of varlist div_ref - div_long_ref {

	esttab pl19_`v' plp19_`v' pl18_`v' plp18_`v' using ${DDD}/Regression/Placebo_DDD.tex, drop(*year* *group* *month*) se obslast nodepvars ///
	title("Placebo - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("2019 (abs.)" "2019 (pop.)" "2018 (abs.)" "2018 (pop.)") page append

}

exit

