***************************************************************
*PRELIMINARIES
***************************************************************
clear all
capture log close
set more off
set scheme lean2

* Enter the location
global location = "/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_etal_2019"

* Enter the location of the do files
global do_files = `"$location/do_files/"'

* Enter the location of the data files
global data_files = `"$location/data_files/"'

* Enter the location of the data files
global support_files = `"$location/support_files/"'

* Enter the location of the raw data
global graphs = `"$location/graphs"'

* Enter the location of the raw data
global tables = `"$location/tables"'

* Change directory
cd "$location"

/***************
LOAD DATA
***************/

use "$data_files/balanced.dta", clear

/***************
CLEAN
***************/

keep id survey month treatment uemr_cat uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk 

/***************
MODEL
***************/

replace survey = 1 if survey>1

*create data set of observations per unemployment reason by treatment group and period (baseline vs. study period)
preserve
	gen obs = 1
	tempfile temp1    /* create a temporary file */
	collapse (sum) N=obs, by(survey treatment uemr_cat)
	replace N = . if uemr_cat == 0
	sort survey treatment uemr_cat
	by survey treatment: egen total = total(N)
	replace N = total if uemr_cat == 0
	label define uemr_cat 0 "Any reason", modify
	drop total
	rename uemr_cat reason
	reshape wide N, i(reason survey) j(treatment)
	
	fillin reason survey
	drop _fillin
	
	replace N0 = 0 if N0 == .
	replace N1 = 0 if N1 == .

	rename N0 N_c
	rename N1 N_t
	
	save "`temp1'"      /* save memory into the temporary file */
restore

foreach var of varlist uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk {

	reg `var' i.treatment##i.survey

	predict yhat_`var'
	predict se_yhat_`var', stdp

	gen did_`var' = _b[1.treatment#1.survey]
	gen se_did_`var' = _se[1.treatment#1.survey]

	foreach t of numlist 0/1 {
		reg `var' i.survey if treatment == `t'

		gen diff_`t'_`var' = _b[1.survey]
		gen se_diff_`t'_`var' = _se[1.survey]
	}

	gen diff_`var' = diff_0_`var' if treatment == 0
	replace diff_`var' = diff_1_`var' if treatment == 1

	gen se_diff_`var' = se_diff_0_`var' if treatment == 0
	replace se_diff_`var' = se_diff_1_`var' if treatment == 1
	
	drop diff_0* diff_1* se_diff_0* se_diff_1*
}



collapse did_* se_did_* yhat_* se_yhat_* diff_* se_diff_*, by(treatment survey)

reshape long se_did_ did_ yhat_ se_yhat_ diff_ se_diff_, i(survey treatment) j(type) string

gen lb_did = did_ - 1.96*se_did_
gen ub_did = did_ + 1.96*se_did_
gen lb_yhat = yhat_ - 1.96*se_yhat_
gen ub_yhat = yhat_ + 1.96*se_yhat_

gen reason = 0 if type == "uemr_any"
replace reason = 1 if type == "uemr_job"
replace reason = 2 if type == "uemr_fam"
replace reason = 3 if type == "uemr_laidoff"
replace reason = 4 if type == "uemr_vacation"
replace reason = 5 if type == "uemr_edu"
replace reason = 6 if type == "uemr_didnot"
replace reason = 7 if type == "uemr_hlth"
replace reason = 8 if type == "uemr_self"
replace reason = 9 if type == "uemr_retire"
replace reason = 10 if type == "uemr_unk"
label define uemr_cat 0 "Any reason", modify
label values reason uemr_cat

preserve
	keep survey treatment reason did se_did yhat se_yhat diff se_diff
	tostring treatment, replace
	replace treatment = "c" if treatment == "0"
	replace treatment = "t" if treatment == "1"

	reshape wide yhat se_yhat did se_did diff se_diff, i(reason survey) j(treatment) string

	drop did_t se_did_t
	rename se_did_c se_did
	rename did_c did
	order survey reason yhat_c se_yhat_c diff_c se_diff_c yhat_t se_yhat_t diff_t se_diff_t did se_did

	sort reason survey 
	by reason : replace diff_c = . if _n == 1 
	by reason : replace se_diff_c = . if _n == 1 
	by reason : replace diff_t = . if _n == 1 
	by reason : replace se_diff_t = . if _n == 1 
	by reason : replace did = . if _n == 1 
	by reason : replace se_did = . if _n == 1 
	
	label define survey 0 "Baseline"
	label define survey 1 "Study period", add
	label values survey survey

	merge m:1 survey reason using "`temp1'", nogen
	sort reason survey 
	
	foreach var of varlist yhat_* se_yhat_* did* se_did* diff_* se_diff_* {
		replace `var' = round(`var',.001)
	}

	export excel using "$tables/d_in_d_bal_table.xls", sheet("full sample") sheetreplace firstrow(variables)
restore

#delimit;
	twoway 
	scatter reason did_ if survey == 0 & treatment == 0, ysc(reverse) ||
	rcap lb_did ub_did reason if survey == 0 & treatment == 0, horizontal
	xline(0)
	plotregion(lcolor(gs0)) 	
	xlabel(-.4(.1).4, grid glcolor(gs12)) xscale(r(-.45, .45))
	ylabel(0(1)10, valuelabel angle(0) grid glcolor(gs12))
	legend(off)
	ytitle("")
;
#delimit cr
graph export "$graphs/did_bal.png", replace

/*
keep survey treatment reason yhat se_yhat
expandcl 11, cluster(treatment reason) generate(newcv)
bysort reason treatment survey: replace newcv = _n
replace newcv = 0 if survey == 0
duplicates drop _all, force
drop if newcv > 0 & newcv < 4
drop survey
rename newcv survey

gen plb = yhat - 1.96*se
gen pub = yhat + 1.96*se



preserve
	drop if reason == 0
	#delimit;
		twoway 
		scatter yhat survey if treatment == 0 & survey == 0, by(reason, note("")  cols(2)) mcolor(gs0) msize(medium)  || 
		scatter yhat survey if treatment == 1 & survey == 0, by(reason, note(""))  mcolor(gs12) msize(medium) ||
		line yhat survey if treatment == 0 & survey > 3,  by(reason, note("")) lcolor(gs0) lwidth(thick) lpattern(solid) || 
		line yhat survey if treatment == 1 & survey > 3,  by(reason, note("")) lcolor(gs12) lwidth(thick) lpattern(solid) ||

		rcap pub plb survey if treatment == 0 & survey > 3,  by(reason, note("")) lcolor(gs0) || 
		rcap pub plb survey if treatment == 1 & survey > 3,  by(reason, note("")) lcolor(gs12) 

		xtitle("Survey wave", size(small))
		ytitle("Frequency (%)", size(small))
		
		subtitle(, lcolor(gs0) lpattern(solid))	plotregion(lcolor(gs0)) 
		xlabel(0 4(1)11, grid glcolor(gs12)) 
		ylabel(0(.1).3, glcolor(gs12)) yscale(r(., .3))
		legend(order(3 "Control" 4 "Treatment") pos(6) row(1))
	;
	#delimit cr
	graph export "$graphs/did_appendix.png", replace 
restore

preserve
	keep if reason == 0
	#delimit;
		twoway 
		scatter yhat survey if treatment == 0 & survey == 0, by(reason, note("")) mcolor(gs0) msize(medium)  || 
		scatter yhat survey if treatment == 1 & survey == 0, by(reason, note(""))  mcolor(gs12) msize(medium) ||
		line yhat survey if treatment == 0 & survey > 3,  by(reason, note("")) lcolor(gs0) lwidth(thick) lpattern(solid) || 
		line yhat survey if treatment == 1 & survey > 3,  by(reason, note("")) lcolor(gs12) lwidth(thick) lpattern(solid) ||

		rcap pub plb survey if treatment == 0 & survey > 3,  by(reason, note("")) lcolor(gs0) || 
		rcap pub plb survey if treatment == 1 & survey > 3,  by(reason, note("")) lcolor(gs12) 

		xtitle("Survey wave", size(small))
		ytitle("Frequency (%)", size(small))
		
		subtitle(, lcolor(gs0) lpattern(solid))	plotregion(lcolor(gs0)) 
		xlabel(0 4(1)11, grid glcolor(gs12)) 
		ylabel(0(.2)1, glcolor(gs12)) yscale(r(., 1)) 
		legend(order(3 "Control" 4 "Treatment") pos(6) row(1))
	;
	#delimit cr
	graph export "$graphs/did_any_appendix.png", replace
restore

