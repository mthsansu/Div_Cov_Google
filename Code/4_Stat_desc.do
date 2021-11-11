*** DESCRIPTIVE STATISTICS

*********************************************************
* This is the stata script designed to do
* descriptive statistics
*********************************************************

set more off 

****************************
***   Preliminary Graph  ***
****************************

* Aggregated data (yearly) for 2004-2018
use ${raw_data}/data_agg.dta, clear

so geo year

egen group = group(geo)
gen geo_lab = substr(geo,4,2) 

labmask group, values(geo)

ta  geo, gen(geo)

set matsize 6000

xtset group year

* Heterogeneity across states
bysort group: egen div_rate_mean_state = mean(div_rate)
twoway scatter div_rate group , msymbol(smcircle) msize(1) mcolor(black) || /// 
connected div_rate_mean_state group , msymbol(diamond) msize(1.5) mcolor(red) lpattern("-") lcolor(red) mlabel(geo_lab) mlabsize(1) mlabposition(0) mlabcolor(white) ///
graphregion(color(white)) bgcolor(white) ///
legend(label(1 "Crude divorce rates by state" "between 2004 and 2018") label(2 "Mean value for each state")  ///
order(1 2) symxsize(8) cols(2) colgap(1)) ///
xtitle("State") ytitle("Crude divorce rate") xlabel(none)
graph export ${stat_desc}/Time_series/Time_series_div_rate_state.pdf , replace as(pdf)


use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist alimony_ref {
	egen `v'_mean = mean(`v') , by(date)
}
foreach v of varlist alimony_ref{
	local ts_`v' `ts_`v'' (line `v'_mean date if geo == "US_NY", lcolor(black)) ||
	local  ts_`v' `ts_`v'' (line `v' date if geo == "US_FL" , lcolor(red) lpattern("-"))

	twoway `ts_`v'', ///
	graphregion(color(white)) bgcolor(white) ///
	legend(label(1 "Averaged time series") label(2 "Florida time series")  ///
	order(1 2) symxsize(8) cols(2) colgap(1)) ///
	xscale(range(16071 22189)) xlabel(16437 18263 19450 20455 21915, format(%tdNN/CCYY)) ///
	xtitle("Date") ytitle("Deviation to the mean number of searches") ///
	ylabel(,angle(0)) tline(02apr2013)
	graph export ${stat_desc}/Time_series/Time_series_GT_`v'_Fl.pdf , replace as(pdf)
}


use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist div_ref - div_ref{
	egen `v'_mean = mean(`v') , by(date)
}
qui levelsof geo , local(levels)
foreach v of varlist div_ref - div_ref{
	foreach l of local levels {
		local  ts_`v' `ts_`v'' (line `v' date if geo == "`l'" & `v' < 250 & year < 2009, lcolor(gs15) lwidth(thin)) || 
	}
	foreach l of local levels {
		local  ts_`v' `ts_`v'' (line `v' date if geo == "`l'" & `v' < 250 & year > 2008 & year < 2020, lcolor(gs13) lwidth(thin)) || 
	}
	foreach l of local levels {
		local  ts_`v' `ts_`v'' (line `v' date if geo == "`l'" & `v' < 250 & year > 2019, lcolor(gs12) lwidth(thin)) || 
	}
	local ts_`v' `ts_`v'' (line `v'_mean date if geo == "US_NY", lcolor(black))

	twoway `ts_`v'', ///
	graphregion(color(white)) bgcolor(white) ///
	legend(label(52 "States time series") label(154 "Averaged time series")  ///
	order(52 154) symxsize(8) cols(2) colgap(1)) ///
	xscale(range(16071 22189)) xlabel(16437 18263 20089 21915, format(%tdNN/CCYY)) ///
	xtitle("Date") ytitle("Deviation to the mean number of searches") ///
	yscale (range(0 250)) ylabel(0(50)250,angle(0)) tline(01jan2009) tline(01jan2020)
	graph export ${stat_desc}/Time_series/Time_series_GT_`v'_scaled.pdf , replace as(pdf)
}



use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist div_ref - div_ref{
	egen `v'_mean = mean(`v') if year < 2020 , by(month)
	egen `v'_2020 = mean(`v') if year == 2020 , by(month)
}
qui levelsof geo , local(levels)
foreach v of varlist div_ref - div_ref{
	foreach l of local levels {
		forv y = 2004/2019{
				local  ts2020_`v' `ts2020_`v'' (line `v' month if year == `y' & geo == "`l'" & `v' < 200, lcolor(gs15) lwidth(thin)) || 
		}
	}
	foreach l of local levels {
			local  ts2020_`v' `ts2020_`v'' (line `v' month if year == 2020 & geo == "`l'" , lcolor(gs11) lwidth(thin)) || 
	}

	local ts2020_`v' `ts2020_`v'' (line `v'_mean month if year == 2019 & geo == "US_NY", lcolor(blue) lwidth(thick)) || ///
	(line `v'_2020 month if year == 2020 & geo == "US_NY", lcolor(red) lwidth(thick))

	twoway `ts2020_`v'', ///
	graphregion(color(white)) bgcolor(white) ///
	legend(label(1 "States series 2004-2019") label(817 "States series 2020") label(868 "Averaged series 2004-2019") label(869 "Averaged series 2020")  ///
	order(1 868 817 869) symxsize(8) cols(2) colgap(1)) ///
	xscale(range(1 12)) xlabel(1(1)12) yscale(range(0 200)) ylabel(0(50)200,angle(0)) ///
	xtitle("Month") ytitle("Deviation to the mean number of searches")
	graph export ${stat_desc}/Years_comparison/Comparison_GT_`v'_scaled.pdf , replace as(pdf)
}


*******************************
***  GT global time series  ***
*******************************

use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist div_ref - div_long_ref{
	egen `v'_mean = mean(`v') , by(date)
}
qui levelsof geo , local(levels)
foreach v of varlist div_ref - div_long_ref{
	foreach l of local levels {
		local  ts_`v' `ts_`v'' (line `v' date if geo == "`l'" , lcolor(gs15) lwidth(thin)) || 
	}
	local ts_`v' `ts_`v'' (line `v'_mean date if geo == "US_NY", lcolor(black))

	twoway `ts_`v'', ///
	graphregion(color(white)) bgcolor(white) ///
	legend(label(1 "States time series") label(52 "Averaged time series")  ///
	order(1 52) symxsize(8) cols(2) colgap(1)) ///
	xscale(range(16071 22189)) xlabel(16437 18263 20089 21915, format(%tdNN/CCYY)) ///
	xtitle("Date") ytitle("Deviation to the mean number of searches") ///
	ylabel(,angle(0)) tline(01jan2020)
	graph export ${stat_desc}/Time_series/Time_series_GT_`v'.pdf , replace as(pdf)
}

*******************************
*** Years Comparison (all)  ***
*******************************

use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist div_ref - div_long_ref{
	egen `v'_mean = mean(`v') if year < 2020 , by(month)
	egen `v'_2020 = mean(`v') if year == 2020 , by(month)
}
qui levelsof geo , local(levels)
foreach v of varlist div_ref - div_long_ref{
	foreach l of local levels {
		forv y = 2004/2019{
				local  ts2020_`v' `ts2020_`v'' (line `v' month if year == `y' & geo == "`l'" , lcolor(gs15) lwidth(thin)) || 
		}
	}
	foreach l of local levels {
			local  ts2020_`v' `ts2020_`v'' (line `v' month if year == 2020 & geo == "`l'" , lcolor(gs11) lwidth(thin)) || 
	}

	local ts2020_`v' `ts2020_`v'' (line `v'_mean month if year == 2019 & geo == "US_NY", lcolor(blue) lwidth(thick)) || ///
	(line `v'_2020 month if year == 2020 & geo == "US_NY", lcolor(red) lwidth(thick))

	twoway `ts2020_`v'', ///
	graphregion(color(white)) bgcolor(white) ///
	legend(label(1 "States series 2004-2019") label(817 "States series 2020") label(868 "Averaged series 2004-2019") label(869 "Averaged series 2020")  ///
	order(1 868 817 869) symxsize(8) cols(2) colgap(1)) ///
	xscale(range(1 12)) xlabel(1(1)12) ///
	xtitle("Month") ytitle("Deviation to the mean number of searches") ///
	ylabel(,angle(0))
	graph export ${stat_desc}/Years_comparison/Comparison_GT_`v'.pdf , replace as(pdf)
}

*******************************
***  Lock States Comparison ***
*******************************

use  ${raw_data}/data_gtrends.dta, clear 

foreach v of varlist div_ref - div_long_ref{
	egen `v'_mean = mean(`v') if year < 2020 , by(month)
	egen `v'_mean_lock = mean(`v') if year < 2020 & has_lockdown == 1, by(month)
	egen `v'_mean_nolock = mean(`v') if year < 2020 & has_lockdown == 0 , by(month)
	egen `v'_2020 = mean(`v') if year == 2020 , by(month)
	egen `v'_2020_lock = mean(`v') if year == 2020 & has_lockdown == 1, by(month)
	egen `v'_2020_nolock = mean(`v') if year == 2020 & has_lockdown == 0, by(month)
}
foreach v of varlist div_ref - div_long_ref{
	local call_`v' (line `v'_mean month if year == 2019 & geo == "US_NY", lcolor(gs1) lpattern("l")) || ///
	(line `v'_mean_lock month if year == 2019 & geo == "US_NY", lcolor(gs1) lpattern("_")) ///
	(line `v'_mean_nolock month if year == 2019 & geo == "US_AR", lcolor(gs1) lpattern(".-.")) ///
	(line `v'_2020 month if year == 2020 & geo == "US_NY", lcolor(red) lpattern("l")) ///
	(line `v'_2020_lock month if year == 2020 & geo == "US_NY", lcolor(red) lpattern("_")) ///
	(line `v'_2020_nolock month if year == 2020 & geo == "US_AR", lcolor(red) lpattern(".-."))

	twoway `call_`v'', /// 
	graphregion(color(white)) bgcolor(white) xscale(range(1 12)) xlabel(1(1)12) ///
	legend(label(1 "Average 2004-2019") label(2 "Average 2004-2019" "(lockdown states)") label(3 "Average 2004-2019" "(no-lockdown states)") ///
	label(4 "Average 2020") label(5 "Average 2020" "(lockdown states)") label(6 "Average 2020" "(no-lockdown states)") order(1 4 2 5 3 6) symxsize(8) cols(2) colgap(1)) ///
	xtitle("Month") ytitle("Deviation to the mean number of searches") ///
	ylabel(,angle(0)) 
	graph export ${stat_desc}/Years_comparison/Lock_comparison_GT_`v'.pdf , replace as(pdf)
}


*******************************
***   Divorce time series   ***
*******************************

use  ${raw_data}/data_gtrends.dta, clear 

preserve
keep if year < 2019
egen div_rate_mean = mean(div_rate) , by(year)

qui levelsof geo , local(levels)
foreach l of local levels {
	local  call_div `call_div' (line div_rate year if geo == "`l'" , lcolor(gs12) lwidth(thin)) || 
}
local call_div `call_div' (connected div_rate_mean year if geo == "US_NY", msymbol(smcircle) mcolor(black) lcolor(black) )

twoway `call_div', ///
graphregion(color(white)) bgcolor(white) ///
legend(label(1 "States rates") label(52 "Averaged rate")  ///
order(1 52) symxsize(8) cols(2) colgap(1)) ///
xscale(range(2004 2018)) xlabel(2004(2)2018) ///
xtitle("Year") ytitle("Crude divorce rate") ///
ylabel(1(1)7,angle(0))
graph export ${stat_desc}/Time_series/Time_series_div_rate.pdf , replace as(pdf)
restore

exit

*******************************
***    Stats Thresholds     ***
*******************************

use ${raw_data}/data_gtrends.dta, clear

so geo date

ta  geo, gen(geo)
egen group = group(geo)
replace under_lockdown = 0 if under_lockdown ==.
gen nb1_under_lockdown = 1 if under_lockdown > 0
replace nb1_under_lockdown = 0 if nb1_under_lockdown ==.
egen nb_under_lockdown = mean(nb1_under_lockdown) if year > 2019 , by(month)
replace nb_under_lockdown = nb_under_lockdown * 51
replace nb_under_lockdown = 0 if nb_under_lockdown ==.


forval x = 0(500)50000 {
	replace after_cases = 0
	replace after_cases = 1 if confirmed > `x'
	egen nb_`x'_aft_cases = mean(after_cases) if year > 2019 , by(month)
	replace nb_`x'_aft_cases = nb_`x'_aft_cases * 51
	replace nb_`x'_aft_cases = 0 if nb_`x'_aft_cases ==.
	gen threshold_`x' = `x'
}

local  call_nb_pand `call_nb_pand' (connected nb_0_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || ///
(scatter nb_0_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_0) mlabcolor(black) mlabsize(1.5)) || 
forval x = 1000(2000)5000 {
	local  call_nb_pand `call_nb_pand' (connected nb_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pand `call_nb_pand' (scatter nb_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_`x') mlabcolor(black) mlabsize(1.5)) || 
}
forval x = 15000(5000)25000 {
	local  call_nb_pand `call_nb_pand' (connected nb_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pand `call_nb_pand' (scatter nb_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_`x') mlabcolor(black) mlabsize(1.5)) || 
}
forval x = 40000(10000)50000 {
	local  call_nb_pand `call_nb_pand' (connected nb_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pand `call_nb_pand' (scatter nb_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_`x') mlabcolor(black) mlabsize(1.5)) || 
}
local  call_nb_pand `call_nb_pand' (connected nb_10000_aft_cases month if geo == "US_NY" & year == 2020, msymbol(circle) msize(2) mcolor(blue) lcolor(blue) mlabel(nb_10000_aft_cases) mlabcolor(white) mlabposition(0) mlabsize(1.5)) || ///
(scatter nb_10000_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_10000) mlabcolor(blue) mlabsize(1.5)) || ///
(connected nb_under_lockdown month if geo == "US_NY" & year == 2020, msymbol(S) msize(2) mcolor(red) lcolor(red) mlabel(nb_under_lockdown) mlabcolor(white) mlabsize(1.5) mlabposition(0))
twoway `call_nb_pand', ///
legend(label(1 "Number of states above the threshold""of new cases per month") ///
label(19 "Number of states above""the choosen threshold") label(21 "Number of states with at least""one day under lockdown in the month") ///
order(1 19 21) symxsize(8) cols(2) colgap(1)) ///
graphregion(color(white)) bgcolor(white) ///
xscale(range(1 10.5)) xlabel(1(1)10) ///
xtitle("Month") ytitle("Number of states")
graph export ${stat_desc}/Graphs/Pandemic_threshold_confirmed.pdf , replace as(pdf)


forval x = 0(100)10000 {
	replace after_cases = 0
	replace after_cases = 1 if confirmed_pop > `x'
	egen nb_pop_`x'_aft_cases = mean(after_cases) if year > 2019 , by(month)
	replace nb_pop_`x'_aft_cases = nb_pop_`x'_aft_cases * 51
	replace nb_pop_`x'_aft_cases = 0 if nb_pop_`x'_aft_cases ==.
	gen threshold_pop_`x' = `x'
}
forval x = 0(1000)1000 {
	local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pop_pand `call_nb_pop_pand' (scatter nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_`x') mlabcolor(black) mlabsize(1.5) mlabposition(2)) || 
}
forval x = 500(1000)1500 {
	local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pop_pand `call_nb_pop_pand' (scatter nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_`x') mlabcolor(black) mlabsize(1.5)) || 
}
forval x = 2000(500)2500{
	local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pop_pand `call_nb_pop_pand' (scatter nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_`x') mlabcolor(black) mlabsize(1.5)) || 
}
forval x = 3500(500)4000{
	local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pop_pand `call_nb_pop_pand' (scatter nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_`x') mlabcolor(black) mlabsize(1.5)) || 
}
forval x = 5000(1000)8000 {
	local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020, msymbol(smcircle) msize(1) mcolor(black) lcolor(black) lwidth(thin)) || 
	local  call_nb_pop_pand `call_nb_pop_pand' (scatter nb_pop_`x'_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_`x') mlabcolor(black) mlabsize(1.5)) || 
}

local  call_nb_pop_pand `call_nb_pop_pand' (connected nb_pop_3000_aft_cases month if geo == "US_NY" & year == 2020, msymbol(circle) msize(2) mcolor(blue) lcolor(blue) mlabel(nb_pop_3000_aft_cases) mlabcolor(white) mlabposition(0) mlabsize(1.5)) || ///
(scatter nb_pop_3000_aft_cases month if geo == "US_NY" & year == 2020 & month == 10, msymbol(none) mlabel(threshold_pop_3000) mlabcolor(blue) mlabsize(1.5)) || ///
(connected nb_under_lockdown month if geo == "US_NY" & year == 2020, msymbol(S) msize(2) mcolor(red) lcolor(red) mlabel(nb_under_lockdown) mlabcolor(white) mlabsize(1.5) mlabposition(0))
twoway `call_nb_pop_pand', ///
legend(label(1 "Number of states above the threshold""of new cases per month""for one million inhabitants") ///
label(25 "Number of states above""the choosen threshold") label(27 "Number of states with at least""one day under lockdown in the month") ///
order(1 25 27) symxsize(8) cols(2) colgap(1)) ///
graphregion(color(white)) bgcolor(white) ///
xscale(range(1 10.5)) xlabel(1(1)10) ///
xtitle("Month") ytitle("Number of states")
graph export ${stat_desc}/Graphs/Pandemic_threshold_confirmed_pop.pdf , replace as(pdf)




*** Descriptive Statistics on Indexes

use  ${raw_data}/data_agg.dta, clear

qui levelsof geo , local(levels)

foreach v of varlist div_rate wedd_rate div_wedd_rate{
egen `v'_mean_NorthEast = mean(`v') if US_NorthEast == 1 , by(year)
egen `v'_mean_West = mean(`v') if US_West == 1 , by(year)
egen `v'_mean_MidWest = mean(`v') if US_MidWest == 1 , by(year)
egen `v'_mean_South = mean(`v') if US_South == 1 , by(year)
egen `v'_mean_US = mean(`v') , by(year)
egen `v'_mean_lock = mean(`v') if has_lockdown == 1 , by(year)
egen `v'_mean_nolock = mean(`v') if has_lockdown == 0 , by(year)
}

foreach v of varlist div_rate wedd_rate div_wedd_rate{
foreach l of local levels {
		local  call_`v' `call_`v'' (line `v' year if geo == "`l'" , lcolor(gs12) lwidth(thin)) || 
	}		

local call_1_`v' `call_`v'' (line `v'_mean_NorthEast year if geo == "US_MA", lcolor(blue)) || ///
(line `v'_mean_West year if geo == "US_AZ", lcolor(green)) ///
(line `v'_mean_MidWest year if geo == "US_IA", lcolor(yellow)) ///
(line `v'_mean_South year if geo == "US_DE", lcolor(ltblue)) ///
(line `v'_mean_US year if geo == "US_NY", lcolor(red) lwidth(thick))

twoway `call_1_`v'', legend(off) ///
title("Index: `v' - United States Regions") graphregion(color(white)) bgcolor(white) xscale(range(2004 2018)) xlabel(2004(1)2018)
graph export ${stat_desc}/Indexes/1_StatDesc_`v'.pdf , replace as(pdf)

local call_2_`v' `call_`v'' (line `v'_mean_lock year if geo == "US_NY", lcolor(blue)) || ///
(line `v'_mean_nolock year if geo == "US_AR", lcolor(green)) ///
(line `v'_mean_US year if geo == "US_NY", lcolor(red) lwidth(thick))

twoway `call_2_`v'', legend(off) ///
title("Index: `v' - United States lockdown treatment") graphregion(color(white)) bgcolor(white) xscale(range(2004 2018)) xlabel(2004(1)2018)
graph export ${stat_desc}/Indexes/2_StatDesc_`v'.pdf , replace as(pdf)

}

use  ${raw_data}/data_agg.dta, clear
drop if geo == "US_NV"

qui levelsof geo , local(levels)

foreach v of varlist div_rate wedd_rate div_wedd_rate{
egen `v'_mean_NorthEast = mean(`v') if US_NorthEast == 1 , by(year)
egen `v'_mean_West = mean(`v') if US_West == 1 , by(year)
egen `v'_mean_MidWest = mean(`v') if US_MidWest == 1 , by(year)
egen `v'_mean_South = mean(`v') if US_South == 1 , by(year)
egen `v'_mean_US = mean(`v') , by(year)
egen `v'_mean_lock = mean(`v') if has_lockdown == 1 , by(year)
egen `v'_mean_nolock = mean(`v') if has_lockdown == 0 , by(year)
}

foreach v of varlist div_rate wedd_rate div_wedd_rate{
foreach l of local levels {
		local  call_NV_`v' `call_NV_`v'' (line `v' year if geo == "`l'" , lcolor(gs12) lwidth(thin)) || 
	}		

local call_NV_1_`v' `call_NV_`v'' (line `v'_mean_NorthEast year if geo == "US_MA", lcolor(blue)) || ///
(line `v'_mean_West year if geo == "US_AZ", lcolor(green)) ///
(line `v'_mean_MidWest year if geo == "US_IA", lcolor(yellow)) ///
(line `v'_mean_South year if geo == "US_DE", lcolor(ltblue)) ///
(line `v'_mean_US year if geo == "US_NY", lcolor(red) lwidth(thick))

twoway `call_NV_1_`v'', legend(off) ///
title("Index: `v' - United States Regions") graphregion(color(white)) bgcolor(white) xscale(range(2004 2018)) xlabel(2004(1)2018)
graph export ${stat_desc}/Indexes/3_StatDesc_NV_`v'.pdf , replace as(pdf)

local call_NV_2_`v' `call_NV_`v'' (line `v'_mean_lock year if geo == "US_NY", lcolor(blue)) || ///
(line `v'_mean_nolock year if geo == "US_AR", lcolor(green)) ///
(line `v'_mean_US year if geo == "US_NY", lcolor(red) lwidth(thick))

twoway `call_NV_2_`v'', legend(off) ///
title("Index: `v' - United States lockdown treatment") graphregion(color(white)) bgcolor(white) xscale(range(2004 2018)) xlabel(2004(1)2018)
graph export ${stat_desc}/Indexes/4_StatDesc_NV_`v'.pdf , replace as(pdf)

} 




exit




