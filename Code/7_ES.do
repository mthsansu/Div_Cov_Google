*** ES ANALYSIS

/*
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

gen confirmed_pop_reg = confirmed_pop / 1000
gen deaths_pop_reg = deaths_pop / 1000


label variable under_lockdown "Lockdown"
label variable confirmed_pop_reg "Cases (pop.)"
label variable deaths_pop_reg "Deaths (pop.)"

gen month_to_lockdown=.
by geo: replace month_to_lockdown = numeric_date*month_under_lockdown
by geo: replace month_to_lockdown = . if month_to_lockdown == 0
by geo: egen min = min(month_to_lockdown)
by geo: replace month_to_lockdown = numeric_date - min
drop min
ta  month_to_lockdown, gen(mtl)
recode mtl* (.=0)
drop mtl203

*** The 196th dummy corresponds to the reference date

forv k = 1/202{
local l = `k' - 196
label var mtl`k' "`l'"
}

****************************
***     ES analysis      ***
***       LOCKDOWN       ***
***  Main specification  ***
****************************

set matsize 6000

global spe_ES "mtl195 - mtl202 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if has_lockdown == 1, cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
	
	restore
	
	xi : reg `v' $spe_ES if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	local N = e(N)
	local Nc = e(N_clust)
	
	coefplot ES_main, keep(mtl*) vertical xtitle("Months since start of the lockdown") yline(0) note("N=`N', N clusters=`Nc'")  ///
	graphregion(color(white)) bgcolor(white) ytitle("Coefficient value")
	graph export ${ES}/Graphs/Lockdown/ES_lock_`v'.pdf , replace as(pdf)
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_lock.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main Lockdown - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}


****************************
***     ES analysis      ***
***    PANDEMIC (abs.)   ***
***  Main specification  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed > 10000
bysort geo: egen has_cases = max(after_cases)

gen cases_cohort1 = 0
gen cases_cohort2 = 0

levelsof geo if after_cases == 1 & month < 7 , local(levels)
foreach l of local levels {
	replace cases_cohort1 = 1 if geo == "`l'"
}
levelsof geo if after_cases == 1 & month < 9 & cases_cohort1 == 0 , local(levels)
foreach l of local levels {
	replace cases_cohort2 = 1 if geo == "`l'"
}

gen month_to_cases=.
by geo: replace month_to_cases = numeric_date*after_cases
by geo: replace month_to_cases = . if month_to_cases == 0
by geo: egen min = min(month_to_cases)
by geo: replace month_to_cases = numeric_date - min
drop min
qui ta  month_to_cases, gen(mtc)
recode mtc* (.=0)
ta month_to_cases if cases_cohort1 == 1
drop mtc207 - mtc209

*** The 202nd dummy corresponds to the reference date

forv k = 1/206{
local l = `k' - 202
label var mtc`k' "`l'"
}

****************************
***       COHORT 1       ***
****************************
set matsize 6000

global spe_ES "mtc201 - mtc206 ib(4).month i.year i.group"
global spe_ES_lin "mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtc201 - mtc206 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtc201 - mtc206 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths

	restore
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	local N = e(N)
	local Nc = e(N_clust)
	
	coefplot ES_main, keep(mtc*) vertical xtitle("Months since exceeding the pandemic threshold") yline(0) note("N=`N', N clusters=`Nc'")  ///
	graphregion(color(white)) bgcolor(white) ytitle("Coefficient value")
	graph export ${ES}/Graphs/Pandemic1/Abs/ES_pand1_`v'.pdf , replace as(pdf)
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pand1.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main Pandemic cohort 1 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (abs.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

****************************
***       COHORT 2       ***
****************************

drop mtc206

set matsize 6000

global spe_ES "mtc200 - mtc205 ib(4).month i.year i.group"
global spe_ES_lin "mtc200 - mtc205 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtc200 - mtc205 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtc200 - mtc205 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtc200 - mtc205 ib(4).month i.year i.group deaths_pop_reg"

qui foreach v of varlist div_ref - div_long_ref {
	
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
	
	restore
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	local N = e(N)
	local Nc = e(N_clust)
	
	coefplot ES_main, keep(mtc*) vertical xtitle("Months since exceeding the pandemic threshold") yline(0) note("N=`N', N clusters=`Nc'")  ///
	graphregion(color(white)) bgcolor(white) ytitle("Coefficient value")
	graph export ${ES}/Graphs/Pandemic2/Abs/ES_pand2_`v'.pdf , replace as(pdf)
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pand2.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main Pandemic cohort 2 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (abs.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases cases_cohort1 cases_cohort2 month_to_cases
drop mtc*


****************************
***     ES analysis      ***
***    PANDEMIC (pop.)   ***
***  Main specification  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed_pop > 3000
bysort geo: egen has_cases = max(after_cases)

gen cases_cohort1 = 0
gen cases_cohort2 = 0

levelsof geo if after_cases == 1 & month < 7 , local(levels)
foreach l of local levels {
	replace cases_cohort1 = 1 if geo == "`l'"
}
levelsof geo if after_cases == 1 & month < 9 & cases_cohort1 == 0 , local(levels)
foreach l of local levels {
	replace cases_cohort2 = 1 if geo == "`l'"
}

gen month_to_cases=.
by geo: replace month_to_cases = numeric_date*after_cases
by geo: replace month_to_cases = . if month_to_cases == 0
by geo: egen min = min(month_to_cases)
by geo: replace month_to_cases = numeric_date - min
drop min
qui ta  month_to_cases, gen(mtc)
recode mtc* (.=0)
ta month_to_cases if cases_cohort1 == 1
drop mtc207 - mtc209

*** The 202nd dummy corresponds to the reference date

forv k = 1/206{
local l = `k' - 202
label var mtc`k' "`l'"
}

****************************
***       COHORT 1       ***
****************************
set matsize 6000

global spe_ES "mtc201 - mtc206 ib(4).month i.year i.group"
global spe_ES_lin "mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtc201 - mtc206 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtc201 - mtc206 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1, cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
	
	restore
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	local N = e(N)
	local Nc = e(N_clust)
	
	coefplot ES_main, keep(mtc*) vertical xtitle("Months since exceeding the pandemic threshold") yline(0) note("N=`N', N clusters=`Nc'")  ///
	graphregion(color(white)) bgcolor(white) ytitle("Coefficient value")
	graph export ${ES}/Graphs/Pandemic1/Pop/ES_pop_pand1_`v'.pdf , replace as(pdf)
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pop_pand1.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main Pandemic cohort 1 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (pop.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

****************************
***       COHORT 2       ***
****************************

drop mtc205 mtc206

set matsize 6000

global spe_ES "mtc200 - mtc204 ib(4).month i.year i.group"
global spe_ES_lin "mtc200 - mtc204 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtc200 - mtc204 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtc200 - mtc204 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtc200 - mtc204 ib(4).month i.year i.group deaths_pop_reg"

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths

	restore
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	local N = e(N)
	local Nc = e(N_clust)
	
	coefplot ES_main, keep(mtc*) vertical xtitle("Months since exceeding the pandemic threshold") yline(0) note("N=`N', N clusters=`Nc'")  ///
	graphregion(color(white)) bgcolor(white) ytitle("Coefficient value")
	graph export ${ES}/Graphs/Pandemic2/Pop/ES_pop_pand2_`v'.pdf , replace as(pdf)
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pop_pand2.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main Pandemic cohort 2 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (pop.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases cases_cohort1 cases_cohort2 month_to_cases
drop mtc*




****************************
***     ES analysis      ***
***   Double ES (abs.)   ***
***  Main specification  ***
****************************

forv k = 1/202{
local l = `k' - 196
label var mtl`k' "mtl`l'"
}

replace after_cases = 0
replace after_cases = 1 if confirmed > 10000
bysort geo: egen has_cases = max(after_cases)

gen cases_cohort1 = 0
gen cases_cohort2 = 0

levelsof geo if after_cases == 1 & month < 7 , local(levels)
foreach l of local levels {
	replace cases_cohort1 = 1 if geo == "`l'"
}
levelsof geo if after_cases == 1 & month < 9 & cases_cohort1 == 0 , local(levels)
foreach l of local levels {
	replace cases_cohort2 = 1 if geo == "`l'"
}

gen month_to_cases=.
by geo: replace month_to_cases = numeric_date*after_cases
by geo: replace month_to_cases = . if month_to_cases == 0
by geo: egen min = min(month_to_cases)
by geo: replace month_to_cases = numeric_date - min
drop min
qui ta  month_to_cases, gen(mtc)
recode mtc* (.=0)
ta month_to_cases if cases_cohort1 == 1
drop mtc207 - mtc209

*** The 202nd dummy corresponds to the reference date

forv k = 1/206{
local l = `k' - 202
label var mtc`k' "mtc`l'"
}

****************************
***       COHORT 1       ***
****************************

set matsize 6000

global spe_ES "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
		
	restore

	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
			
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_lock_pand1.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main cohort 1 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (abs.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

****************************
***       COHORT 2       ***
****************************

drop mtc206

set matsize 6000

global spe_ES "mtl195 - mtl202 mtc200 - mtc205 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 mtc200 - mtc205 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 mtc200 - mtc205 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 mtc200 - mtc205 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 mtc200 - mtc205 ib(4).month i.year i.group deaths_pop_reg"

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
		
	restore

	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_lock_pand2.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main cohort 2 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (abs.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases cases_cohort1 cases_cohort2 month_to_cases
drop mtc*


****************************
***     ES analysis      ***
***   Double ES (pop.)   ***
***  Main specification  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed_pop > 3000
bysort geo: egen has_cases = max(after_cases)

gen cases_cohort1 = 0
gen cases_cohort2 = 0

levelsof geo if after_cases == 1 & month < 7 , local(levels)
foreach l of local levels {
	replace cases_cohort1 = 1 if geo == "`l'"
}
levelsof geo if after_cases == 1 & month < 9 & cases_cohort1 == 0 , local(levels)
foreach l of local levels {
	replace cases_cohort2 = 1 if geo == "`l'"
}

gen month_to_cases=.
by geo: replace month_to_cases = numeric_date*after_cases
by geo: replace month_to_cases = . if month_to_cases == 0
by geo: egen min = min(month_to_cases)
by geo: replace month_to_cases = numeric_date - min
drop min
qui ta  month_to_cases, gen(mtc)
recode mtc* (.=0)
ta month_to_cases if cases_cohort1 == 1
drop mtc207 - mtc209

*** The 202nd dummy corresponds to the reference date

forv k = 1/206{
local l = `k' - 202
label var mtc`k' "`l'"
}

****************************
***       COHORT 1       ***
****************************
set matsize 6000

global spe_ES "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 mtc201 - mtc206 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort1 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
			
	restore

	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pop_lock_pand1.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main cohort 1 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (pop.)" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

****************************
***       COHORT 2       ***
****************************

drop mtc205 mtc206

set matsize 6000

global spe_ES "mtl195 - mtl202 mtc200 - mtc204 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 mtc200 - mtc204 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 mtc200 - mtc204 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 mtc200 - mtc204 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 mtc200 - mtc204 ib(4).month i.year i.group deaths_pop_reg"

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if cases_cohort2 == 1 , cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
			
	restore

	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_pop_lock_pand2.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("Main cohort 2 - Var = `v'") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main (pop.)" "Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

drop has_cases cases_cohort1 cases_cohort2 month_to_cases
drop mtc*
*/

****************************
***    Output Results    ***
****************************

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
gen confirmed_pop_reg = confirmed_pop / 1000
gen deaths_pop_reg = deaths_pop / 1000
label variable under_lockdown "Lockdown"
label variable confirmed_pop_reg "Cases (pop.)"
label variable deaths_pop_reg "Deaths (pop.)"

gen month_to_lockdown=.
by geo: replace month_to_lockdown = numeric_date*month_under_lockdown
by geo: replace month_to_lockdown = . if month_to_lockdown == 0
by geo: egen min = min(month_to_lockdown)
by geo: replace month_to_lockdown = numeric_date - min
drop min
ta  month_to_lockdown, gen(mtl)
recode mtl* (.=0)
drop mtl203

*** The 196th dummy corresponds to the reference date

forv k = 1/202{
local l = `k' - 196
label var mtl`k' "`l'"
}

****************************
***     ES analysis      ***
***       LOCKDOWN       ***
***  Main specification  ***
****************************

set matsize 6000

global spe_ES "mtl195 - mtl202 ib(4).month i.year i.group"

foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_`v'
 	restore	
}

esttab ES_lock_ref ES_div_ref ES_div_law_ref ES_child_support_ref ES_alimony_ref ES_div_how_ref ES_div_papers_ref ES_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the Statewide Lockdowns") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab ES_div_legal_ref ES_div_lawyer_ref ES_child_custody_ref ES_div_file_ref ES_div_much_ref ES_div_long_ref ES_div_cov_ref ES_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the Statewide Lockdowns") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append



****************************
***     ES analysis      ***
***    PANDEMIC (pop.)   ***
***  Main specification  ***
****************************

replace after_cases = 0
replace after_cases = 1 if confirmed_pop > 3000
bysort geo: egen has_cases = max(after_cases)
gen cases_cohort1 = 0
gen cases_cohort2 = 0
levelsof geo if after_cases == 1 & month < 7 , local(levels)
foreach l of local levels {
	replace cases_cohort1 = 1 if geo == "`l'"
}
levelsof geo if after_cases == 1 & month < 9 & cases_cohort1 == 0 , local(levels)
foreach l of local levels {
	replace cases_cohort2 = 1 if geo == "`l'"
}

gen month_to_cases=.
by geo: replace month_to_cases = numeric_date*after_cases
by geo: replace month_to_cases = . if month_to_cases == 0
by geo: egen min = min(month_to_cases)
by geo: replace month_to_cases = numeric_date - min
drop min
qui ta  month_to_cases, gen(mtc)
recode mtc* (.=0)
ta month_to_cases if cases_cohort1 == 1
drop mtc207 - mtc209

*** The 202nd dummy corresponds to the reference date

forv k = 1/206{
local l = `k' - 202
label var mtc`k' "`l'"
}

****************************
***       COHORT 1       ***
****************************
set matsize 6000

global spe_ES "mtc201 - mtc206 ib(4).month i.year i.group" 

qui foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_`v'
	restore

}

esttab ES_lock_ref ES_div_ref ES_div_law_ref ES_child_support_ref ES_alimony_ref ES_div_how_ref ES_div_papers_ref ES_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the First Wave of the Pandemic") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab ES_div_legal_ref ES_div_lawyer_ref ES_child_custody_ref ES_div_file_ref ES_div_much_ref ES_div_long_ref ES_div_cov_ref ES_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the First Wave of the Pandemic") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append

****************************
***       COHORT 2       ***
****************************

drop mtc205 mtc206

set matsize 6000

global spe_ES "mtc200 - mtc204 ib(4).month i.year i.group"

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_`v'
	restore
}

esttab ES_lock_ref ES_div_ref ES_div_law_ref ES_child_support_ref ES_alimony_ref ES_div_how_ref ES_div_papers_ref ES_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the Second Wave of the Pandemic") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab ES_div_legal_ref ES_div_lawyer_ref ES_child_custody_ref ES_div_file_ref ES_div_much_ref ES_div_long_ref ES_div_cov_ref ES_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study on GT Indicators depending on the Second Wave of the Pandemic") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append

****************************
***     DDD analysis     ***
***       LOCKDOWN       ***
***  Main specification  ***
****************************

gen aft_cases_underlock = after_cases*under_lockdown
label variable after_cases "Pandemic Threshold"
label variable aft_cases_underlock "Lockdown * Pandemic Threshold"

set matsize 6000

global spe_DDD "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group" 

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008

	xi : reg `v' $spe_DDD [aw=pop_mean] , cluster(geo)
	est store DDD_`v'
	restore
}

esttab DDD_lock_ref DDD_div_ref DDD_div_law_ref DDD_child_support_ref DDD_alimony_ref DDD_div_how_ref DDD_div_papers_ref DDD_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD on GT Indicators") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab DDD_div_court_ref DDD_div_lawyer_ref DDD_alimony_ref DDD_lock_ref DDD_div_cov_media_ref DDD_div_file_ref DDD_div_much_ref DDD_div_long_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD on GT Indicators") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append


****************************
***     ES analysis      ***
***       LOCKDOWN       ***
*** Heterogeneous Effects***
****************************

set matsize 6000

global spe_ES "mtl195 - mtl202 ib(4).month i.year i.group"

foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if has_lockdown == 1 & cases_cohort1 == 1 [aw=pop_mean], cluster(geo)
	est store ES_`v'
 	restore	
}

esttab ES_lock_ref ES_div_ref ES_div_law_ref ES_child_support_ref ES_alimony_ref ES_div_how_ref ES_div_papers_ref ES_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study with Heterogeneous Effects: First Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab ES_div_legal_ref ES_div_lawyer_ref ES_child_custody_ref ES_div_file_ref ES_div_much_ref ES_div_long_ref ES_div_cov_ref ES_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study with Heterogeneous Effects: First Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append

foreach v of varlist div_ref - div_long_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if has_lockdown == 1 & cases_cohort2 == 1 [aw=pop_mean], cluster(geo)
	est store ES_`v'
 	restore	
}

esttab ES_lock_ref ES_div_ref ES_div_law_ref ES_child_support_ref ES_alimony_ref ES_div_how_ref ES_div_papers_ref ES_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study with Heterogeneous Effects: Secon Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab ES_div_legal_ref ES_div_lawyer_ref ES_child_custody_ref ES_div_file_ref ES_div_much_ref ES_div_long_ref ES_div_cov_ref ES_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Event Study with Heterogeneous Effects: Second Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append

****************************
***     DDD analysis     ***
***       LOCKDOWN       ***
*** Heterogeneous Effects***
****************************

set matsize 6000

global spe_DDD "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group" 

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008

	xi : reg `v' $spe_DDD if cases_cohort1 == 1 [aw=pop_mean] , cluster(geo)
	est store DDD_`v'
	restore
}

esttab DDD_lock_ref DDD_div_ref DDD_div_law_ref DDD_child_support_ref DDD_alimony_ref DDD_div_how_ref DDD_div_papers_ref DDD_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD with Heterogeneous Effects: First Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab DDD_div_legal_ref DDD_div_lawyer_ref DDD_child_custody_ref DDD_div_file_ref DDD_div_much_ref DDD_div_long_ref DDD_div_cov_ref DDD_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD with Heterogeneous Effects: First Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append

qui foreach v of varlist div_ref - div_long_ref {

	preserve
	keep if year > 2008

	xi : reg `v' $spe_DDD if cases_cohort2 == 1 [aw=pop_mean] , cluster(geo)
	est store DDD_`v'
	restore
}

esttab DDD_lock_ref DDD_div_ref DDD_div_law_ref DDD_child_support_ref DDD_alimony_ref DDD_div_how_ref DDD_div_papers_ref DDD_div_court_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD with Heterogeneous Effects: Second Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("lock" "div" "div_law" "child_support" "alimony" "div_how" "div_papers"  "div_court") ///
page append

esttab DDD_div_legal_ref DDD_div_lawyer_ref DDD_child_custody_ref DDD_div_file_ref DDD_div_much_ref DDD_div_long_ref DDD_div_cov_ref DDD_div_cov_media_ref ///
using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("DDD with Heterogeneous Effects: Second Wave") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div_legal" "div_lawyer" "child_custody" "div_file" "div_much" "div_long" "div_cov" "div_cov_media") ///
page append


****************************
***     ES analysis      ***
***       LOCKDOWN       ***
***      Robustness      ***
****************************

set matsize 6000

global spe_ES "mtl195 - mtl202 ib(4).month i.year i.group"
global spe_ES_lin "mtl195 - mtl202 ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_ES_lin_quad "mtl195 - mtl202 ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_ES_conf "mtl195 - mtl202 ib(4).month i.year i.group confirmed_pop_reg" 
global spe_ES_deaths "mtl195 - mtl202 ib(4).month i.year i.group deaths_pop_reg" 

qui foreach v of varlist div_ref {
	
	preserve
	keep if year > 2008
	xi : reg `v' $spe_ES if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_main
	
	xi : reg `v' $spe_ES if has_lockdown == 1, cluster(geo)
	est store ES_nopop
	
	xi : reg `v' $spe_ES_lin if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin
	
	xi : reg `v' $spe_ES_lin_quad if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_lin_quad
	
	xi : reg `v' $spe_ES_conf if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_conf
	
	xi : reg `v' $spe_ES_deaths if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_deaths
	
	restore
	
	xi : reg `v' $spe_ES if has_lockdown == 1 [aw=pop_mean], cluster(geo)
	est store ES_2004
 	
	esttab ES_main ES_nopop ES_lin ES_lin_quad ES_conf ES_deaths ES_2004 using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month* tl* *tq*) se obslast nodepvars ///
	title("Event Study Robustness for the GT Indicator div") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("Main" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}

global spe_DDD "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group" 
global spe_DDD_lin "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group tl_geo1 - tl_geo51" 
global spe_DDD_lin_quad "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group tl_geo1 - tq_geo51" 
global spe_DDD_conf "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group confirmed_pop_reg" 
global spe_DDD_deaths "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group deaths_pop_reg" 

****************************
***     DDD analysis     ***
***  Robusteness (pop.)  ***
****************************

set matsize 6000

qui foreach v of varlist div_ref {

	preserve
	keep if year > 2008
	xi : reg `v' $spe_DDD  [aw=pop_mean], cluster(geo)
	est store DDD_main
	
	xi : reg `v' $spe_DDD  , cluster(geo)
	est store DDD_nopop
	
	xi : reg `v' $spe_DDD_lin  [aw=pop_mean], cluster(geo)
	est store DDD_lin
	
	xi : reg `v' $spe_DDD_lin_quad  [aw=pop_mean], cluster(geo)
	est store DDD_lin_quad
	
	xi : reg `v' $spe_DDD_conf  [aw=pop_mean], cluster(geo)
	est store DDD_conf
	
	xi : reg `v' $spe_DDD_deaths  [aw=pop_mean], cluster(geo)
	est store DDD_deaths
	
	restore
	
	xi : reg `v' $spe_DDD  [aw=pop_mean], cluster(geo)
	est store DDD_2004
	
	esttab DDD_main DDD_nopop DDD_lin DDD_lin_quad DDD_conf DDD_deaths DDD_2004 using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month* tl* tq*) se obslast nodepvars ///
	title("DDD Robustness for the GT Indicator div") nonumbers label compress longtable nogaps r2 noomitted ///
	mtitles("DDD" "No Weight" "Linear" "Quadratic" "Cases Control" "Deaths Control" "Rob 2004") page append
	
}


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

qui foreach v of varlist div_ref div_law_ref{
	
	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed_pop > 3000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl  [aw=pop_mean], cluster(geo)
	est store plp19_`v'
	
	drop pl_has_cases	
}

restore
	
by geo: replace pl_under_lockdown = under_lockdown[_n+24]
by geo: replace pl_confirmed = confirmed[_n+24]
by geo: replace pl_confirmed_pop = confirmed_pop[_n+24]
	
preserve
drop if year > 2019
drop if year == 2019 & month > 10
	
qui foreach v of varlist div_ref - div_long_ref {
	
	replace pl_after_cases = 0
	replace pl_after_cases = 1 if pl_confirmed_pop > 3000
	bysort geo: egen pl_has_cases = max(pl_after_cases)
	replace pl_aft_cases_underlock = pl_after_cases*pl_under_lockdown

	xi : reg `v' $spe_pl  [aw=pop_mean], cluster(geo)
	est store plp18_`v'
	
	drop pl_has_cases

}	
restore

esttab plp19_div_ref plp18_div_ref plp19_div_law_ref plp18_div_law_ref using ${ES}/Regression/ES_lock_all_main.tex, drop(*year* *group* *month*) se obslast nodepvars ///	
title("Placebo lockdown for the years 2018 and 2019") nonumbers label compress longtable nogaps r2 noomitted ///
mtitles("div 2019" "div 2018" "div_law 2019" "div_law 2018") page append


exit
