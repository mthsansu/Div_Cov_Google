***** STATIC PANEL ANALYSIS

/*
* Aggregated data (yearly) for 2004-2018
use ${raw_data}/data_agg.dta, clear

set more off

so geo year

egen group = group(geo)
gen geo_lab = substr(geo,4,2) 
labmask group, values(geo)

gen cst = 1

rename child_support* c_support*
rename child_custody* c_custody*
rename unempl_annual* unempl_ann*

qui ta  geo, gen(geo)
forv k = 2004/2020{
	gen y`k' = (year ==`k')
}

*** Generate state-specific trends (linear, then quadratic)
so geo year
by geo: gen trend = _n
forv k = 1/44{
gen tl_geo`k' = trend*geo`k'
}
forv k = 1/44{
gen tq_geo`k' = trend*trend*geo`k'
}

* Because of the lags we predict on years 2005 to 2018
drop if year == 2004

/*
* Screen on the number of states where the variable is available
scalar screen = 84
qui foreach v of varlist div_ref - div_long_ref div_ref_lag1 - div_long_ref_lead6 {
	count if missing(`v')
	if r(N) > screen {
		drop `v'
	}
}
*/

****************************
***     Static Panel     ***
***   Forward selection  ***
*** Cross-validated MSPE ***
****************************

set matsize 8000

xtset group year

* Variables lists

* Variable list with all GT predictors
*global varlist_GT "div_ref - div_long_ref div_ref_lag1 - div_long_ref_lead6"
* Variable list with the main GT predictors and lags
*global varlist_GT "div_ref div_papers_ref div_court_ref div_law_ref alimony_ref c_support_ref div_how_ref div_ref_lag1 - div_ref_lag11 div_papers_ref_lag1 - div_papers_ref_lag11 div_court_ref_lag1 - div_law_ref_lag11 alimony_ref_lag1 - c_support_ref_lag11 div_how_ref_lag1 - div_how_ref_lag11"
*Variable list wit the unempl variables
global varlist_GT "unempl_ann_rate - unempl_ann_rate_lag11"

* Regression specification for the selection
global spe_select "sex_ratio med_age div_ref div_law_ref_lag7 div_ref_lag11 unempl_ann_rate"

* 44 states with rates available estimated on 2 years (44x2=88)
scalar mean_base = 88
scalar mean_MSPE = 88
gen var_mean_base = mean_base
gen var_mean_MSPE = mean_MSPE

foreach v of varlist $spe_select {
	count if missing(`v')
	scalar mean_base`v' = 88 - r(N)*2/14
    if mean_base`v' < mean_base {
	scalar mean_base = mean_base`v'
	}
	di mean_base`v'
}	
di mean_base
replace var_mean_base = mean_base

* For each variable
qui foreach v of varlist $varlist_GT {
	count if missing(`v')
	scalar mean_MSPE = 88 - r(N)*2/14
    if mean_MSPE > mean_base {
	scalar mean_MSPE = mean_base
	}
	replace var_mean_MSPE = mean_MSPE
	* For each year
	forv y = 2005/2017{
		* Two-year window of prediction
		gen pred_time = 1
		replace pred_time = 0 if year != `y' & year != `y'+1

		* Static Panel Model: the most simple one with states fixed-effects
		xi: xtreg div_rate $spe_select if year != `y' & year != `y'+1 , fe cluster(geo)
		* Linear prediction without the error component (idiosyncratic + remainder)
		predict xb_base, xb 
		* Prediction of the idiosyncratic error component = fixed-effect
		predict eff_base , u
		* Manipulations to add the idiosyncratic error component as needed (which is constant over time for a state)
		if `y' == 2005 {
			replace eff_base = eff_base[_n+2] if missing(eff_base)
		}
		else {
			replace eff_base = eff_base[_n-1] if missing(eff_base)
		}
		* Final prediction of the divorce rate based on the selection specification regression
		gen y_base = xb_base + eff_base
		* Since we observe the actual divorce rates, we could compute the prediction error
		gen err_base = div_rate - y_base

		* Mean Square Predicted Error
		gen MSPE_1_base = sum(err_base^2*pred_time)/ var_mean_base
		egen MSPE_base = max(MSPE_1_base)

		* GT Static Panel Model: we add only one variable of the list of GT variables  and do the regression
		* We will further compare it to our baseline model
		xi: xtreg div_rate $spe_select `v' if year != `y' & year != `y'+1 , fe cluster(geo)
		* Same operations as above then
		predict xb_GT , xb
		predict eff_GT , u
		if `y' == 2005 {
			replace eff_GT = eff_GT[_n+2] if missing(eff_GT)
		}
		else {
			replace eff_GT = eff_GT[_n-1] if missing(eff_GT)
		}
		gen y_GT = xb_GT + eff_GT
		gen err_GT = div_rate - y_GT
		
		* MSPE
		gen MSPE_1_GT = sum(err_GT^2*pred_time)/ var_mean_MSPE
		egen MSPE_GT = max(MSPE_1_GT)

		* Calculate the gains in terms of percentage for each metrics
		
		gen MSPE_b`y' = MSPE_base
		gen MSPE`v'`y' = MSPE_GT

		drop pred_time xb_base eff_base y_base err_base MSPE_1_base MSPE_base xb_GT eff_GT y_GT err_GT MSPE_1_GT MSPE_GT

	}
	
	* Generate the mean of the percentage gains on the different window for each metrics

	egen MSPE_m_`v' = rowmean(MSPE_b*)
	egen MSPE_mean`v' = rowmean(MSPE`v'*)

	drop MSPE_b* MSPE`v'*
}

* Show which variable induces the best score for the metrics

egen MSPE_best = rowmin(MSPE_mean*)

gen var_best_MSPE = ""

qui foreach v of varlist $varlist_GT {
	replace var_best_MSPE = "`v'" if MSPE_mean`v' == MSPE_best

}

*tab MSPE_m_div_ref
tab MSPE_m_unempl_ann_rate
tab var_best_MSPE MSPE_best

*drop MSPE_m*

exit
*/

****************************
***     Panel Graphs     ***
****************************

* Aggregated data (yearly) for 2004-2020
*use ${raw_data}/data_pred.dta, clear
use ${raw_data}/data_pred_19.dta, clear

set more off

so geo year

egen group = group(geo)
gen geo_lab = substr(geo,4,2) 
labmask group, values(geo)
bysort group: carryforward med_age, replace
bysort group: carryforward sex_ratio, replace
egen pop_mean = mean(pop), by(group)
bysort group: carryforward pop_mean, replace

gen cst = 1

rename child_support* c_support*
rename child_custody* c_custody*
rename unempl_annual* unempl_ann*

ta  geo, gen(geo)
forv k = 2004/2020{
	gen y`k' = (year ==`k')
}

*** Generate state-specific trends (linear, then quadratic)
so geo year
by geo: gen trend = _n
forv k = 1/44{
gen tl_geo`k' = trend*geo`k'
}
forv k = 1/44{
gen tq_geo`k' = trend*trend*geo`k'
}

drop if year == 2004
carryforward age2_m , replace 

drop if geo == "US_AK" | geo == "US_VT"

set matsize 8000

global spe_GT "sex_ratio med_age div_ref div_law_ref_lag7 div_ref_lag11"
global spe_unempl "sex_ratio med_age div_ref div_law_ref_lag7 div_ref_lag11 unempl_ann_rate"

xtset group year

*xi: xtreg div_rate $spe_GT if year < 2019, fe cluster(geo)
xi: xtreg div_rate $spe_GT if year < 2019 [aw=pop_mean], fe cluster(geo)
predict xb_GT, xb
predict eff_GT , u
predict CIse_GT , stdp
replace eff_GT = eff_GT[_n-2] if missing(eff_GT)
gen y_GT = xb_GT + eff_GT
generate lowerCI_GT = y_GT - 1.645*CIse_GT
generate upperCI_GT = y_GT + 1.645*CIse_GT

*xi: xtreg div_rate $spe_unempl , fe cluster(geo)
xi: xtreg div_rate $spe_unempl [aw=pop_mean], fe cluster(geo)
predict xb_unempl, xb
predict eff_unempl , u
predict CIse_unempl , stdp
replace eff_unempl = eff_unempl[_n-2] if missing(eff_unempl)
gen y_unempl = xb_unempl + eff_unempl
generate lowerCI_unempl = y_unempl - 1.645*CIse_unempl
generate upperCI_unempl = y_unempl + 1.645*CIse_unempl



levelsof geo , local(levels)
foreach l of local levels {

twoway (rarea lowerCI_GT upperCI_GT year if geo == "`l'", color(gs14)) || ///
(rarea lowerCI_unempl upperCI_unempl year if geo == "`l'" , color(gs14)) || ///
(connected div_rate year if geo == "`l'" & year >2017, msymbol(circle) msize(1) mcolor(green) lcolor(green) lpattern("-")) || ///
(connected div_rate year if geo == "`l'" & year <2019, msymbol(circle) msize(1) mcolor(black) lcolor(black)) || ///  
(connected y_GT year if geo == "`l'" & year <2019, msymbol(smcircle) msize(1) mcolor(blue) lcolor(blue)) || ///
(connected y_unempl year if geo == "`l'" & year <2019, msymbol(smcircle) msize(1) mcolor(red) lcolor(red)) || ///
(connected y_GT year if geo == "`l'" & year > 2017, msymbol(smcircle) msize(1) mcolor(blue) lcolor(blue) lpattern("-")) || ///
(connected y_unempl year if geo == "`l'" & year > 2017, msymbol(smcircle) msize(1) mcolor(red) lcolor(red) lpattern("-")) , ///
graphregion(color(white)) bgcolor(white) ///
legend(label(1 "90% confidence interval") label(4 "Crude divorce rate") label(5 "GT prediction") label(6 "With unemployment") ///
order(4 5 6 1) symxsize(8) cols(2) colgap(1)) ///
xtitle("Year") ytitle("Actual and predicted rates") ///
ylabel(,angle(0))
graph export "C:\Users\Mathis\Desktop/Ined\Divorce/Static_panel/19w_`l'.pdf" , replace as(pdf) 
}



so year
by year: egen div_rate_mean = mean(div_rate)
by year: egen y_GT_mean = mean(y_GT)
by year: egen y_unempl_mean = mean(y_unempl)
by year: egen lowerCI_GT_mean = mean(lowerCI_GT)
by year: egen upperCI_GT_mean = mean(upperCI_GT)
by year: egen lowerCI_unempl_mean = mean(lowerCI_unempl)
by year: egen upperCI_unempl_mean = mean(upperCI_unempl)




twoway (rarea lowerCI_GT_mean upperCI_GT_mean year if geo == "US_NY", color(gs14)) || ///
(rarea lowerCI_unempl_mean upperCI_unempl_mean year if geo == "US_NY" , color(gs14)) || ///
(connected div_rate_mean year if geo == "US_NY" & year >2017, msymbol(circle) msize(1) mcolor(green) lcolor(green) lpattern("-")) || ///
(connected div_rate_mean year if geo == "US_NY" & year <2019, msymbol(circle) msize(1) mcolor(black) lcolor(black) lwidth(medthick)) || ///  
(connected y_GT_mean year if geo == "US_NY" & year <2019, msymbol(o) msize(1) mcolor(blue) lcolor(blue)) || ///
(connected y_unempl_mean year if geo == "US_NY" & year <2019, msymbol(o) msize(1) mcolor(red) lcolor(red)) || ///
(connected y_GT_mean year if geo == "US_NY" & year > 2017, msymbol(o) msize(1) mcolor(blue) lcolor(blue) lpattern("-")) || ///
(connected y_unempl_mean year if geo == "US_NY" & year > 2017, msymbol(o) msize(1) mcolor(red) lcolor(red) lpattern("-")) , ///
graphregion(color(white)) bgcolor(white) ///
legend(label(1 "90% confidence interval") label(4 "Averaged crude divorce rate") label(5 "Averaged GT prediction") label(6 "With unemployment") ///
order(4 5 6 1) symxsize(8) cols(2) colgap(1)) ///
xtitle("Year") ytitle("Actual and predicted averaged rates") yscale(range(2 5)) ///
ylabel(2(0.5)5 ,angle(0))
graph export "C:\Users\Mathis\Desktop/Ined\Divorce/Static_panel/19w_Average_pred.pdf" , replace as(pdf) 
 
 
 exit
