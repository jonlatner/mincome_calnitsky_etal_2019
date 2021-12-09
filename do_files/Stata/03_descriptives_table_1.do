/***************
Preliminaries
***************/
clear all
capture log close
net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
set scheme lean2
set more off

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
DESCRIPTIVE TABLE
***************/

tab fam_type, gen(fam_type_)
tab uemr_cat, gen(uemr_cat_)

sort famnum month
by famnum: gen unique_fam = 1 if _n == 1

sort id month
by id: gen unique_n = 1 if _n == 1

sum unique*

sort treatment
estpost summarize male age fam_size fam_type_* uemr_cat_* if treatment == 0, listwise
est sto control

sum unique_n if treatment == 0
estadd scalar obs_n = r(N)

sum unique_fam if treatment == 0
estadd scalar obs_fam = r(N)


estpost summarize male age fam_size fam_type_* uemr_cat_* if treatment == 1, listwise
est sto treatment

sum unique_n if treatment == 1
estadd scalar obs_n = r(N)

sum unique_fam if treatment == 1
estadd scalar obs_fam = r(N)


estpost summarize male age fam_size fam_type_* uemr_cat_*, listwise
est sto both

sum unique_n
estadd scalar obs_n = r(N)

sum unique_fam
estadd scalar obs_fam = r(N)

/***************
CREATE TABLE
***************/

#delimit ;
	esttab control treatment both using "$tables/descriptives_bal.csv", 
	replace
	nostar unstack nonote nomtitle nonumber 
	mlabels("Control" "Treatment" "Full sample")
	varlabels(
	male "Male" 
	age "Age" 
	fam_size "Family size"
	fam_type_1 "Single"  
	fam_type_2 "Single-headed household"  
	fam_type_3 "Dual-headed household" 
	uemr_cat_1 "Employed" 
	uemr_cat_2 "Family" 
	uemr_cat_3 "Job/work conditions" 
	uemr_cat_4 "Laid off" 
	uemr_cat_5 "Unpaid vacation" 
	uemr_cat_6 "Education" 
	uemr_cat_7 "Did not want to work" 
	uemr_cat_8 "Ill or disabled" 
	uemr_cat_9 "Self-employed" 
	uemr_cat_10 "Retired" 
	uemr_cat_11 "Other/Unknown" 
	,blist(
	uemr_cat_2 "`=char(13)' Reason for not working `=char(13)'" 
	))
	collabels(, lhs("Variables"))  
	cells("mean(fmt(%9.3fc) label(Avg.)) sd(fmt(%9.3fc) label(SD) par)") 
	scalar("obs_n Unique observations" "obs_fam Unique families")
;
#delimit cr

drop fam_type_*
drop uemr_cat_*

/*
/***************
DESCRIPTIVE GRAPH - AGGREGATE REASONS
***************/

*create mean
preserve
	tempfile temp_mean    /* create a temporary file */
	tab uemr_cat, gen(uemr__)
	collapse (mean) uemr_any uemr__*, by(treatment survey)
	reshape long uemr__, i(treatment survey) j(reason)
	replace uemr__ = uemr_any if reason == 1
	drop uemr_any
	rename uemr__ mean_uemr__
	save "`temp_mean'"      /* save memory into the temporary file */
restore

*create se
preserve
	tempfile temp_se    /* create a temporary file */
	tab uemr_cat, gen(uemr__)
	collapse (semean) uemr_any uemr__*, by(treatment survey)
	reshape long uemr__, i(treatment survey) j(reason)
	replace uemr__ = uemr_any if reason == 1
	drop uemr_any
	rename uemr__ se_uemr__
	save "`temp_se'"      /* save memory into the temporary file */
restore

*GRAPH ANY REASON
preserve
	use "`temp_mean'", clear
	merge 1:1 treatment survey reason using "`temp_se'", nogen
	
	keep if reason == 1
	
	label define reason 1 "Any reason", modify
	label values reason reason
	
	generate upr = mean + se
	generate lwr = mean - se
	
	#delimit ;
	twoway
	scatter mean_uemr survey if treatment == 0 & survey == 0, by(reason, note("")) mcolor(gs0) msize(medium)  || 
	scatter mean_uemr survey if treatment == 1 & survey == 0, by(reason, note("")) mcolor(gs12) msize(medium) ||
	line mean_uemr survey if treatment == 0  & survey > 3, by(reason, note("")) lcolor(gs0) lwidth(thick) lpattern(solid) ||
	line mean_uemr survey if treatment == 1  & survey > 3, by(reason, note("")) lcolor(gs12) lwidth(thick) lpattern(solid)
	xtitle("Survey wave", size(small))
	ytitle("Frequency (%)", size(small))
	
	legend(order(3 "Control" 4 "Treatment") pos(6) row(1))
	subtitle(, lcolor(gs0) lpattern(solid))	plotregion(lcolor(gs0)) 
	xlabel(0 4(1)11, grid glcolor(gs12)) 
	ylabel(0(.2)1, glcolor(gs12)) yscale(r(., 1))

	;
	#delimit cr
	graph export "$graphs/dscrptv_uemr_any_frequency_line.png", replace
restore

*create graph	
*	rcap upr lwr survey if treatment == 0 & survey > 3, by(reason, note("")) lcolor(gs0) || 
*	rcap upr lwr survey if treatment == 1 & survey > 3, by(reason, note("")) lcolor(gs12) 
*GRAPH SPECIFIC REASONS
preserve
	use "`temp_mean'", clear
	merge 1:1 treatment survey reason using "`temp_se'", nogen
	
	label define reason 1 "Any reason", modify
	label define reason 2 "Family", add
	label define reason 3 "Job/work conditions", add
	label define reason 4 "Laid off", add
	label define reason 5 "Unpaid vacation", add
	label define reason 6 "Education", add
	label define reason 7 "Did not want to work", add
	label define reason 8 "Ill or disabled", add
	label define reason 9 "Self-employed", add
	label define reason 10 "Retired", add
	label define reason 11 "Other/unknown", add
	label values reason reason
	
	drop if reason == 1
	
	generate upr = mean + se
	generate lwr = mean - se
	
	#delimit ;
	twoway
	scatter mean_uemr survey if treatment == 0 & survey == 0, by(reason, note("") cols(2)) mcolor(gs0) msize(medium)  || 
	scatter mean_uemr survey if treatment == 1 & survey == 0, by(reason, note("")) mcolor(gs12) msize(medium) ||
	line mean_uemr survey if treatment == 0  & survey > 3, by(reason, note("")) lcolor(gs0) lwidth(thick) lpattern(solid) ||
	line mean_uemr survey if treatment == 1  & survey > 3, by(reason, note("")) lcolor(gs12) lwidth(thick) lpattern(solid)
	xtitle("Survey wave", size(small))
	ytitle("Frequency (%)", size(small))
	
	legend(order(3 "Control" 4 "Treatment") pos(6) row(1))
	subtitle(, lcolor(gs0) lpattern(solid))	plotregion(lcolor(gs0)) 
	xlabel(0 4(1)11, grid glcolor(gs12)) 
	ylabel(0(.1).3, glcolor(gs12)) yscale(r(., .3)) ysize(8.5)

	;
	#delimit cr
	graph export "$graphs/dscrptv_uemr_agg_frequency_line.png", replace
restore
