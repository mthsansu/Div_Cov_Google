*** THRESHOLDS FOR REGRESSION COEFFICIENTS

use ${raw_data}/data_gtrends.dta, clear

set more off

so geo date

ta  geo, gen(geo)
egen group = group(geo)
egen pop_mean = mean(pop) if month == 1 , by(group)
bysort group: carryforward pop_mean, replace
replace under_lockdown = 0 if under_lockdown ==.
gen nb1_under_lockdown = 1 if under_lockdown > 0
replace nb1_under_lockdown = 0 if nb1_under_lockdown ==.
egen nb_under_lockdown = mean(nb1_under_lockdown) if year > 2019 , by(month)
replace nb_under_lockdown = nb_under_lockdown * 51
replace nb_under_lockdown = 0 if nb_under_lockdown ==.

replace after_cases = 0
gen aft_cases_underlock = after_cases*under_lockdown

*******************************
***  Thresholds Regression  ***
*******************************

global spe_DDD   "under_lockdown after_cases aft_cases_underlock ib(4).month i.year i.group"

foreach v of varlist div_ref - div_long_ref{

tempname myresults_`v' 
postfile `myresults_`v'' threshold coeff_after_cases se_after_cases coeff_under_lockdown se_under_lockdown coeff_aft_cases_underlock se_aft_cases_underlock using ${DDD}/Databases/myresults_`v'.dta 
quietly forval x = 0(500)50000 {
	replace after_cases = 0
	replace after_cases = 1 if confirmed > `x'
	bysort geo: egen has_cases = max(after_cases)
	replace aft_cases_underlock = after_cases*under_lockdown
	preserve
	keep if year > 2008
    xi: regress `v' $spe_DDD [aw=pop_mean] , cluster(geo)
    post `myresults_`v'' (`x') (`=_b[after_cases]') (`=_se[after_cases]') (`=_b[under_lockdown]') (`=_se[under_lockdown]') (`=_b[aft_cases_underlock]') (`=_se[aft_cases_underlock]')
	restore
	drop has_cases
}
postclose `myresults_`v''

tempname myresults_pop_`v' 
postfile `myresults_pop_`v'' threshold coeff_after_cases se_after_cases coeff_under_lockdown se_under_lockdown coeff_aft_cases_underlock se_aft_cases_underlock using ${DDD}/Databases/myresults_pop_`v'.dta
quietly forval x = 0(100)10000 {
	replace after_cases = 0
	replace after_cases = 1 if confirmed_pop > `x'
	bysort geo: egen has_cases = max(after_cases)
	replace aft_cases_underlock = after_cases*under_lockdown
	preserve
	keep if year > 2008
    xi: regress `v' $spe_DDD [aw=pop_mean] , cluster(geo)
    post `myresults_pop_`v'' (`x') (`=_b[after_cases]') (`=_se[after_cases]') (`=_b[under_lockdown]') (`=_se[under_lockdown]') (`=_b[aft_cases_underlock]') (`=_se[aft_cases_underlock]')
	restore
	drop has_cases

}
postclose `myresults_pop_`v''

}

*******************************
***    Thresholds Graphs    ***
*******************************

foreach v in div_ref div_cov_ref div_file_ref div_papers_ref div_legal_ref div_lawyer_ref div_court_ref div_law_ref lock_ref div_cov_media_ref alimony_ref child_support_ref child_custody_ref div_how_ref div_much_ref div_long_ref{

use ${DDD}/Databases/myresults_`v'.dta, clear

twoway (connected coeff_after_cases threshold if threshold > 2999 & threshold < 15001 ,msymbol(smcircle) msize(0.5) mcolor(black) lcolor(black) lwidth(thin) lpattern("-")) || ///
(connected coeff_under_lockdown threshold if threshold > 2999 & threshold < 15001 ,msymbol(smcircle) msize(0.5) mcolor(blue) lcolor(blue) lwidth(thin) lpattern("-")) || ///
(connected coeff_aft_cases_underlock threshold if threshold > 2999 & threshold < 15001 ,msymbol(smcircle) msize(0.5) mcolor(red) lcolor(red) lwidth(thin) lpattern("-")) , ///
legend(label(1 "Coefficient associated with" "the pandemic index") label(2 "Coefficient associated with" "the lockdown") ///
label(3 "Coefficient associated with" "both the pandemic index and lockdown") ///
order(1 2 3) symxsize(8) cols(2) colgap(1)) ///
xscale(range(3000 15000)) xlabel(4000(2000)15000) ///
graphregion(color(white)) bgcolor(white) ///
xtitle("Threshold: new confirmed cases""per month") ytitle("Coefficient value") ///
yline(0) xline(10000) saving(best_coeff_`v'.gph, replace)

use ${DDD}/Databases/myresults_pop_`v'.dta, clear

twoway (connected coeff_after_cases threshold if threshold > 1499 & threshold < 4001 ,msymbol(smcircle) msize(0.5) mcolor(black) lcolor(black) lwidth(thin) lpattern("-")) || ///
(connected coeff_under_lockdown threshold if threshold > 1499 & threshold < 4001 ,msymbol(smcircle) msize(0.5) mcolor(blue) lcolor(blue) lwidth(thin) lpattern("-")) || ///
(connected coeff_aft_cases_underlock threshold if threshold > 1499 & threshold < 4001 ,msymbol(smcircle) msize(0.5) mcolor(red) lcolor(red) lwidth(thin) lpattern("-")) , ///
legend(label(1 "Coefficient of" "the pandemic index") label(2 "Coefficient of" "the lockdown") ///
label(3 "Coefficient of" "the DDD estimator") ///
order(1 2 3) symxsize(4) cols(2) colgap(1)) ///
xscale(range(1500 4000)) xlabel(1500(500)4000) ///
graphregion(color(white)) bgcolor(white) ///
xtitle("Threshold: new confirmed cases""per month for one million inhabitants") /* ytitle("Coefficient value") */ ///
yline(0) xline(3000) saving(best_coeff_pop_`v'.gph, replace)

grc1leg2 best_coeff_`v'.gph best_coeff_pop_`v'.gph , ycommon scheme(s1mono) legendfrom(best_coeff_`v'.gph)

graph export ${DDD}/Graphs/Best_coeff_threshold_`v'.pdf , replace as(pdf)

}

exit



