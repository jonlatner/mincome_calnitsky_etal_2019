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

keep id survey month treatment age uemr_cat uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk

recode age (min/25=1) (26/49=2) (50/max=3), gen(agecat)

tab age agecat

label define agecat 1 "<=25"
label define agecat 2 "26-49", add
label define agecat 3 ">=50", add
label values agecat agecat
tab agecat

/***************
MODEL
***************/

replace survey = 1 if survey>1

*create data set of observations per unemployment reason by treatment group and period (baseline vs. study period)
preserve
	gen obs = 1
	tempfile temp1    /* create a temporary file */
	collapse (sum) N=obs, by(survey agecat uemr_cat)
	replace N = . if uemr_cat == 0
	sort survey agecat uemr_cat
	by survey agecat: egen total = sum(N)
	replace N = total if uemr_cat == 0
	label define uemr_cat 0 "Any reason", modify
	drop total
	rename uemr_cat reason
	reshape wide N, i(reason survey) j(agecat)
	
	fillin reason survey
	drop _fillin

	replace N1 = 0 if N1 == .
	replace N2 = 0 if N2 == .
	replace N3 = 0 if N3 == .

	rename N1 N_25
	rename N2 N_26_49
	rename N3 N_50
	
	save "`temp1'"      /* save memory into the temporary file */
restore

foreach var of varlist uemr_any uemr_job uemr_fam uemr_laidoff uemr_vacation uemr_edu uemr_didnot uemr_hlth uemr_self uemr_retire uemr_unk {
	foreach g of numlist 1/3 {
		reg `var' i.treatment##i.survey if agecat == `g'

		predict yhat_`g'_`var' if agecat == `g'
		predict se_yhat_`g'_`var' if agecat == `g', stdp

		gen did_`g'_`var' = _b[1.treatment#1.survey] if agecat == `g'
		gen se_did_`g'_`var' = _se[1.treatment#1.survey] if agecat == `g'

		foreach t of numlist 0/1 {
			reg `var' i.survey if treatment == `t' & agecat == `g'

			gen diff_`t'_`g'_`var' = _b[1.survey] if treatment == `t' & agecat == `g'
			gen se_diff_`t'_`g'_`var' = _se[1.survey] if treatment == `t' & agecat == `g'
		}
	}
	gen diff_1_`var' = .
	gen diff_2_`var' = .
	gen diff_3_`var' = .

	replace diff_1_`var' = diff_1_1_`var' if treatment == 1 & agecat == 1
	replace diff_1_`var' = diff_0_1_`var' if treatment == 0 & agecat == 1

	replace diff_2_`var' = diff_1_2_`var' if treatment == 1 & agecat == 2
	replace diff_2_`var' = diff_0_2_`var' if treatment == 0 & agecat == 2

	replace diff_3_`var' = diff_1_3_`var' if treatment == 1 & agecat == 3
	replace diff_3_`var' = diff_0_3_`var' if treatment == 0 & agecat == 3

	gen se_diff_1_`var' = .
	gen se_diff_2_`var' = .
	gen se_diff_3_`var' = .

	replace se_diff_1_`var' = se_diff_1_1_`var' if treatment == 1 & agecat == 1
	replace se_diff_1_`var' = se_diff_0_1_`var' if treatment == 0 & agecat == 1

	replace se_diff_2_`var' = se_diff_1_2_`var' if treatment == 1 & agecat == 2
	replace se_diff_2_`var' = se_diff_0_2_`var' if treatment == 0 & agecat == 2

	replace se_diff_3_`var' = se_diff_1_3_`var' if treatment == 1 & agecat == 3
	replace se_diff_3_`var' = se_diff_0_3_`var' if treatment == 0 & agecat == 3
}

drop diff_1_1_* diff_1_2_* diff_1_3_* diff_0_1_* diff_0_2_* diff_0_3_* se_diff_1_1_* se_diff_1_2_* se_diff_1_3_* se_diff_0_1_* se_diff_0_2_* se_diff_0_3_* 

collapse se_yhat_* yhat_* did_* se_did_* diff_* se_diff_*, by(treatment survey)

#delimit;
	reshape long 
	yhat_1_ se_yhat_1_ se_diff_1_ diff_1_ did_1_ se_did_1_ 
	yhat_2_ se_yhat_2_ se_diff_2_ diff_2_ did_2_ se_did_2_ 
	yhat_3_ se_yhat_3_ se_diff_3_ diff_3_ did_3_ se_did_3_ 
	, i(survey treatment) j(type) string
;
#delimit cr


foreach g of numlist 1/3 {
	rename yhat_`g'_ yhat_`g'
	rename se_yhat_`g'_ se_yhat_`g'
	rename did_`g'_ did_`g'
	rename se_did_`g'_ se_did_`g'
	rename diff_`g'_ diff_`g'
	rename se_diff_`g'_ se_diff_`g'
}

reshape long yhat_ se_yhat_ did_ se_did_ diff_ se_diff_, i(survey treatment type) j(agecat)

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
	keep survey treatment reason agecat did se_did yhat se_yhat diff se_diff
	tostring treatment, replace
	replace treatment = "c" if treatment == "0"
	replace treatment = "t" if treatment == "1"

	reshape wide yhat se_yhat did se_did diff se_diff, i(reason survey agecat ) j(treatment) string

	drop did_t se_did_t
	rename se_did_c se_did
	rename did_c did
	order agecat survey reason yhat_c se_yhat_c diff_c se_diff_c yhat_t se_yhat_t diff_t se_diff_t did se_did

	sort agecat reason survey
	by agecat reason : replace diff_c = . if _n == 1 
	by agecat reason : replace se_diff_c = . if _n == 1 
	by agecat reason : replace diff_t = . if _n == 1 
	by agecat reason : replace se_diff_t = . if _n == 1 
	by agecat reason : replace did = . if _n == 1 
	by agecat reason : replace se_did = . if _n == 1 
	
	label define agecat 1 "<=25"
	label define agecat 2 "26-49", add
	label define agecat 3 ">=50", add
	label values agecat agecat

	merge m:1 survey reason using "`temp1'", nogen
	sort reason survey 
	
	foreach var of varlist yhat_* se_yhat_* did* se_did* diff_* se_diff_* {
		replace `var' = round(`var',.001)
	}

	export excel using "$tables/d_in_d_bal_table.xls", sheet("age") sheetreplace firstrow(variables)
restore

keep if survey == 0 & treatment == 0
keep reason treatment did_ lb_did ub_did agecat

foreach g of numlist 1/3 {
	#delimit;
		twoway 
		scatter reason did_ if agecat == `g', ysc(reverse) ||
		rcap lb_did ub_did reason if agecat == `g', horizontal
		xline(0)
		plotregion(lcolor(gs0)) 
		xlabel(-.4(.1).4, grid glcolor(gs12)) xscale(r(-.45, .45))
		ylabel(0(1)10, valuelabel angle(0) labsize(tiny) grid glcolor(gs12))
		legend(off)
		ytitle("")
		xtitle("Difference in Difference (agecat == `g')", size(vsmall))
		saving("$graphs/did_agecat_`g'", replace)
	;
	#delimit cr
}

foreach g of numlist 1/3 {
	#delimit;
		twoway 
		scatter reason did_ if agecat == `g', ysc(reverse) ||
		rcap lb_did ub_did reason if agecat == `g', horizontal
		xline(0)
		plotregion(lcolor(gs0)) 
		xlabel(-.4(.1).4, grid glcolor(gs12)) xscale(r(-.45, .45))
		ylabel(0(1)10, valuelabel angle(0) grid glcolor(gs12))
		legend(off)
		ytitle("")
	;
	#delimit cr
	graph export "$graphs/did_bal_agecat_`g'.png", replace
*	graph export "$graphs/did_bal_agecat_`g'.tif", width(2000) replace
}

/***********
COMBINE GRAPHS
***********/

cd "$graphs"

#delimit ;
graph combine 
did_agecat_1.gph
did_agecat_2.gph
did_agecat_3.gph
, xcommon ycommon col(1) 
;
#delimit cr
graph export "$graphs/did_bal_agecat.png", replace
*graph export "$graphs/Fig 3_DiD by age Reasons for non-employment.tif", width(2000) replace

rm did_agecat_1.gph
rm did_agecat_2.gph
rm did_agecat_3.gph

