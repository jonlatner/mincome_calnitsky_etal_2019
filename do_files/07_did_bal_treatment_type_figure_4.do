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
1: annual guarantee = 3800, negative tax rate = .35
2: annual guarantee = 4800, negative tax rate = .35
3: annual guarantee = 3800, negative tax rate = .50
4: annual guarantee = 4800, negative tax rate = .50
5: annual guarantee = 5800, negative tax rate = .50
6: annual guarantee = 3800, negative tax rate = .75
7: annual guarantee = 4800, negative tax rate = .75
8: annual guarantee = 3800, negative tax rate = .75
9: control group
***************/

keep id survey month type_treatment uemr_cat uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk

gen type = .
replace type = 0 if type_treatment == 9 /*Control*/
replace type = 1 if type_treatment == 1 | type_treatment == 3 | type_treatment == 6 | type_treatment == 8 /*3800*/
replace type = 2 if type_treatment == 2 | type_treatment == 4 | type_treatment == 7 /*4800*/
replace type = 3 if type_treatment == 5 /*5800*/

/***************
MODEL
***************/

replace survey = 1 if survey>1

*create data set of observations per unemployment reason by treatment group and period (baseline vs. study period)
preserve
	gen obs = 1
	tempfile temp1    /* create a temporary file */
	collapse (sum) N=obs, by(survey type uemr_cat)
	replace N = . if uemr_cat == 0
	sort survey type uemr_cat
	by survey type: egen total = total(N)
	replace N = total if uemr_cat == 0
	label define uemr_cat 0 "Any reason", modify
	drop total
	rename uemr_cat reason
	reshape wide N, i(reason survey) j(type)

	fillin reason survey
	drop _fillin

	replace N0 = 0 if N0 == .
	replace N1 = 0 if N1 == .
	replace N2 = 0 if N2 == .
	replace N3 = 0 if N3 == .

	rename N0 N_c
	rename N1 N_3800
	rename N2 N_4800
	rename N3 N_5800
	
	save "`temp1'"      /* save memory into the temporary file */
restore

foreach var of varlist uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk {
	gen yhat_0_`var' = .
	gen se_yhat_0_`var' = .

	foreach g of numlist 1/3 {
		reg `var' i.type##i.survey if type == 0 | type == `g'

		predict pr
		predict pr_se, stdp

		replace yhat_0_`var' = pr if type == 0
		replace se_yhat_0_`var' = pr_se if type == 0

		gen yhat_`g'_`var' = pr if type == `g'
		gen se_yhat_`g'_`var' = pr_se if type == `g'
		
		drop pr*

		gen did_`g'_`var' = _b[`g'.type#1.survey] if type == `g'
		gen se_did_`g'_`var' = _se[`g'.type#1.survey] if type == `g'
		
		reg `var' i.survey if type == `g'

		gen diff_`g'_`var' = _b[1.survey] if type == `g'
		gen se_diff_`g'_`var' = _se[1.survey] if type == `g'
	}
	reg `var' i.survey if type == 0

	gen diff_0_`var' = _b[1.survey] if type == 0
	gen se_diff_0_`var' = _se[1.survey] if type == 0
}



collapse yhat_* se_yhat_* did_* se_did_* diff_* se_diff_*, by(survey)

#delimit;
	reshape long 
	yhat_0_ se_yhat_0_ se_diff_0_ diff_0_ did_0_ se_did_0_ 
	yhat_1_ se_yhat_1_ se_diff_1_ diff_1_ did_1_ se_did_1_ 
	yhat_2_ se_yhat_2_ se_diff_2_ diff_2_ did_2_ se_did_2_ 
	yhat_3_ se_yhat_3_ se_diff_3_ diff_3_ did_3_ se_did_3_ 
	, i(survey) j(reason) string
;
#delimit cr


foreach g of numlist 0/3 {
	rename yhat_`g'_ yhat_`g'
	rename se_yhat_`g'_ se_yhat_`g'
	rename did_`g'_ did_`g'
	rename se_did_`g'_ se_did_`g'
	rename diff_`g'_ diff_`g'
	rename se_diff_`g'_ se_diff_`g'
}

reshape long yhat_ se_yhat_ did_ se_did_ diff_ se_diff_, i(survey reason) j(type)

gen lb_did = did_ - 1.96*se_did_
gen ub_did = did_ + 1.96*se_did_
gen lb_yhat = yhat_ - 1.96*se_yhat_
gen ub_yhat = yhat_ + 1.96*se_yhat_

replace reason = "0" if reason == "uemr_any"
replace reason = "1" if reason == "uemr_job"
replace reason = "2" if reason == "uemr_fam"
replace reason = "3" if reason == "uemr_laidoff"
replace reason = "4" if reason == "uemr_vacation"
replace reason = "5" if reason == "uemr_edu"
replace reason = "6" if reason == "uemr_didnot"
replace reason = "7" if reason == "uemr_hlth"
replace reason = "8" if reason == "uemr_self"
replace reason = "9" if reason == "uemr_retire"
replace reason = "10" if reason == "uemr_unk"
destring reason, replace
label define uemr_cat 0 "Any reason", modify
label values reason uemr_cat

replace type = 3800 if type == 1
replace type = 4800 if type == 2
replace type = 5800 if type == 3

preserve
	keep survey reason type did se_did yhat se_yhat diff se_diff

	reshape wide yhat se_yhat did se_did diff se_diff, i(reason survey) j(t)

	sort reason survey
	foreach g of numlist 0 3800 4800 5800 {
		by reason : replace diff_`g' = . if _n == 1 
		by reason : replace se_diff_`g' = . if _n == 1 
		by reason : replace did_`g' = . if _n == 1 
		by reason : replace se_did_`g' = . if _n == 1 
	}

	drop did_0 se_did_0

	#delimit;
		order survey reason 
		yhat_0 se_yhat_0 diff_0 se_diff_0 
		yhat_3800 se_yhat_3800 diff_3800 se_diff_3800 did_3800 se_did_3800
		yhat_4800 se_yhat_4800 diff_4800 se_diff_4800 did_4800 se_did_4800 
		yhat_5800 se_yhat_5800 diff_5800 se_diff_5800 did_4800 se_did_4800
		
	;
	#delimit cr

	rename yhat_0 yhat_c
	rename se_yhat_0 se_yhat_c
	rename diff_0 diff_c
	rename se_diff_0 se_diff_c

	label define survey 0 "Baseline"
	label define survey 1 "Study period", add
	label values survey survey
	
	merge m:1 survey reason using "`temp1'", nogen
	sort reason survey 
	
	foreach var of varlist yhat_* se_yhat_* did_* se_did_* diff_* se_diff_* {
		replace `var' = round(`var',.001)
	}

	export excel using "$tables/d_in_d_bal_table.xls", sheet("treatment_type") sheetreplace firstrow(variables)
restore

keep if survey == 0
keep reason type did_ lb_did ub_did
drop if type == 0

foreach g of numlist 3800 4800 5800 {
	#delimit;
		twoway 
		scatter reason did_ if type == `g', ysc(reverse) ||
		rcap lb_did ub_did reason if type == `g', horizontal
		xline(0)
		plotregion(lcolor(gs0)) 
		xlabel(-.4(.1).4, grid glcolor(gs12)) xscale(r(-.45, .45))
		ylabel(0(1)10, valuelabel angle(0) labsize(tiny) grid glcolor(gs12))
		legend(off)
		ytitle("")
		xtitle("Difference in Difference (Annual guarantee = $`g')", size(vsmall))
		saving("$graphs/did_treatment_type_`g'", replace)
	;
	#delimit cr
}

foreach g of numlist 3800 4800 5800 {
	#delimit;
		twoway 
		scatter reason did_ if type == `g', ysc(reverse) ||
		rcap lb_did ub_did reason if type == `g', horizontal
		xline(0)
		plotregion(lcolor(gs0)) 
		xlabel(-.4(.1).4, grid glcolor(gs12)) xscale(r(-.45, .45))
		ylabel(0(1)10, valuelabel angle(0) grid glcolor(gs12))
		legend(off)
		ytitle("")
	;
	#delimit cr
	graph export "$graphs/did_bal_treatment_type_`g'.png", replace
*	graph export "$graphs/did_treatment_type_`g'.tif", width(2000) replace
}

/***********
COMBINE GRAPHS
***********/

cd "$graphs"

#delimit ;
graph combine 
did_treatment_type_3800.gph
did_treatment_type_4800.gph 
did_treatment_type_5800.gph
, xcommon ycommon col(1) 
;
#delimit cr
graph export "$graphs/did_bal_treatment_type.png", replace

rm did_treatment_type_3800.gph
rm did_treatment_type_4800.gph
rm did_treatment_type_5800.gph

