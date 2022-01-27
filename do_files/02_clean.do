/***************
Preliminaries
***************/
clear all
capture log close
set more off

* Enter the location
global location = "/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_etal_2019"

* Enter the location of the do files
global do_files = `"$location/do_files/"'

* Enter the location of the data files
global data_files = `"$location/data_files/"'

* Enter the location of the data files
global support_files = `"$location/support_files/"'

* Change directory
cd "$location"

/***************
Merge both data sets
minc1 has 2173 individuals
minc4 has 1290
***************/

use "$data_files/MINC1.dta", clear
keep famnum sitecode agem agef famsize mhnotlook fhnotlook 

/***************
discrepancy in coding of uemr between minc1 and minc4

minc1:
0 = employed
17 = Has job but has not started working
18 = Too difficult to get to town (rural only)
19 = Wanted to help with family farm

minc4:
0 = JPL Note: there is no comparable code for employed, so I must create one later in the code
17 = Temporarily ill or disabled
18 = Permanently ill or disabled
19 = JPL Note: there is no comparable code for "wanted to help with the family farm"

therefore code 17-19 from minc1 into "other" reason
***************/

recode mhnotlook (17/19=19)
recode fhnotlook (17/19=19)



merge 1:1 famnum using "$data_files/MINC4.dta"
keep if _m == 3
drop _m

preserve
	keep famnum di*
	reshape long di, i(famnum) j(survey)
	rename di int_date
	replace int_date = . if int_date == -999
	save "$data_files/int_date.dta", replace
restore


rename plan type_treatment
rename attrit attrition
rename ftypw fam_type
rename ofnnum hd_id

rename hours# hd_hours#
rename uemst# hd_uemst#, renumber
rename uemen# hd_uemnd#, renumber
rename uemr# hd_uemr#, renumber

rename fhours# wf_hours#, renumber
rename fumst# wf_uemst#, renumber
rename fumen# wf_uemnd#, renumber
rename fumr# wf_uemr#, renumber

rename agem hd_age
rename agef wf_age

rename famsize fam_size
rename mhnotlook hd_uemr0
rename fhnotlook wf_uemr0
rename fofnn wf_id

keep famnum type_treatment attrition fam_* hd_* wf_*
reshape long hd_hours wf_hours hd_uemr wf_uemr hd_uemst wf_uemst hd_uemnd wf_uemnd, i(famnum hd_id wf_id type_treatment attrition fam_type fam_size hd_age wf_age hd_id) j(month)

*there are some duplicate ids where head has wife id or wife has head id
replace wf_id = 313702 if wf_id == 313701
replace hd_id = 985501 if hd_id == 985502

*create a data set of head info
preserve
	keep if hd_id>0
	gen male = 1
	keep famnum month male type_treatment attrition fam_* hd*
	rename (hd_id hd_age hd_hours hd_uemst hd_uemnd hd_uemr) (id age hours uemst uemnd uemr)
	save "$data_files/hd.dta", replace
restore

*create a data set of wife (or female head) info
preserve
	keep if wf_id>0
	gen male = 0
	keep famnum month male type_treatment attrition fam_* wf*
	rename (wf_id wf_age wf_hours wf_uemst wf_uemnd wf_uemr) (id age hours uemst uemnd uemr)
	save "$data_files/wf.dta", replace
restore

*combine head and wife data together
use "data_files/hd.dta", clear
append using "data_files/wf.dta"
recode month (1/3=1)(4/6=2)(7/9=3)(10/12=4)(13/15=5)(16/18=6)(19/21=7)(22/24=8)(25/27=9)(28/30=10)(31/33=11), gen(survey)
order famnum id month survey attrition male type_treatment fam_type fam_size age hours uemst uemnd uemr

sort famnum id survey month 
by famnum id survey: gen first_m = 1 if _n == 1
replace first_m = 0 if first_m == .

*merge in interview date
merge m:1 famnum survey using "$data_files/int_date", nogen

*create data set of hours worked per survey period
preserve
	tempfile temp1    /* create a temporary file */
	keep id month hours
	drop if hours == .
	rename month survey
	rename hours test
	save "`temp1'"      /* save memory into the temporary file */
restore

*merge in hours worked per survey period
merge m:1 id survey using "`temp1'", nogen
drop hours
rename test hours
sort id month

/***************
CLEANING
***************/

*count unique observations
sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*
drop unique*

recode type_treatment (0/8=1) (9=0), gen(treatment)
drop if type_treatment < 0

*count unique observations
sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*

* small number of observations with missing age
tab unique_n if age < 1 | age == .
drop if age < 1 | age == .
drop unique*

*count unique observations
sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*
drop unique*

* unemployed for any reason
gen uemr_any = 0 if uemr <= 0
replace uemr_any = 1 if uemr > 0

/***************
ATTRITION
***************/

tostring attrition, replace
gen attrition_survey = substr(attrition,-2,1) /* variable for why household left the experiment (refused, no longer interviewable, moved (address known or unknown), discontinued (other reasons)*/
gen attrition_survey_period = substr(attrition,-1,1) /* variable for when household left the experiment after survey period __*/
destring attrition attrition_survey attrition_survey_period, replace
gen test = 1 if survey > attrition_survey_period & attrition_survey_period > 0 /* variable indicating period after which people stopped participating in the program*/
drop if test == 1 /* drop after people stopped participating in the program*/
drop attrition_* test*
replace attrition = 1 if attrition > 0

/***************
DROPPING SURVEYS (1, 2, 3)
***************/

*from codebook minc4 pg.8 through pg.9: it should be noted that all fields for survey 2 and 3 are set to missing since unemployment reason was not asked at survey 2 and 3
*drop survey period 2 and 3
drop if survey == 2 | survey == 3

*JPL NOTE: in contrast to the other surveys, uemr in suvey 1 less than 1% of all cases provide any reason for being unemployed
*therefore, i drop survey period 1
tab survey uemr_any, row nofreq m
drop if survey == 1

*technically, unemployment reason should be asked for each month in each survey.
*however, it does not appear that this is followed.  for evidence:
tab survey uemr_any, row nofreq m
tab month uemr_any, row nofreq m
tab month uemr_any if first_m == 1, row nofreq m

*i do not do this, but one option is to do the following:
*if they did not provide a reason for not working in current time period and are not working in current time period, 
*but they did provide a reason in the previous time period, then replace current missing reason with previous reason
*sort id survey month
*by id survey: replace uemr = uemr[_n-1] if uemr == -999 & _n > 1 & hours == 0

*instead, i do the following:
*keep only first observation per survey because most other observations are missing
sort famnum id survey month
by famnum id survey: keep if _n == 1

/***************
EMPLOYED
THERE IS NO VARIABLE INDICATING EMPLOYMENT IN MINC4 (SURVEY PERIOD 3 - 11)

two options:
1) assume you are working if do not have a reason for not working
2) assume you are working if you are working (hours > 0) and do not have a reason for not working
***************/

*assume you are working if you are working and do not have a reason for not working
gen test1 = 1 if uemr == -999 & survey > 3
gen test2 = 1 if hours > 0 & uemr == -999 & survey > 3

*only small difference between the two
sum test*
drop test*

*therefore, i use the first definition, which is the most general definition
replace uemr = 0 if uemr == -999 & survey > 3

/***************
UNEMPLOYMENT REASON
***************/

*there are a small number of cases with 0 hours, but no unemployment reason
tab uemr if survey > 3

* Job/work conditions = "2. Labor dispute" | "4. No jobs available" | "12. Available wages too low"
gen uemr_job = 1 if uemr == 2 | uemr == 4 | uemr == 12 
replace uemr_job = 0 if uemr == 0

* Family reasons = "6. Wanted to take care of the family" | "7. Child-care too expensive" | "8. Pregnancy," 
gen uemr_fam = 1 if uemr == 6 | uemr == 7 | uemr == 8
replace uemr_fam = 0 if uemr == 0

* "1. Laid off"
gen uemr_laidoff = 1 if uemr == 1
replace uemr_laidoff = 0 if uemr == 0

* "3. Unpaid vacation"
gen uemr_vacation = 1 if uemr == 3
replace uemr_vacation = 0 if uemr == 0

* Education = "9. In job training" | "10. In school"
gen uemr_edu = 1 if uemr == 9 | uemr == 10
replace uemr_edu = 0 if uemr == 0

* "13. Did not want to work" 
gen uemr_didnot = 1 if uemr == 13
replace uemr_didnot = 0 if uemr == 0

* Health = "14. Ill or disabled" | "17. Temporarily ill or disabled" | "18. Permanently ill or disabled"
gen uemr_hlth = 1 if uemr == 14 | uemr == 17 | uemr == 18
replace uemr_hlth = 0 if uemr == 0

* "15. Self-employed"
gen uemr_self = 1 if uemr == 15
replace uemr_self = 0 if uemr == 0

* "16. Retired"
gen uemr_retire = 1 if uemr == 16
replace uemr_retire = 0 if uemr == 0

* unknown = unknown or "5. Bad weather" or "other" or "11. institutionalized"
gen uemr_unk = 1 if uemr == 99 | uemr == 5 | uemr == 19 | uemr == 11
replace uemr_unk = 0 if uemr == 0 

gen uemr_cat = 0 if uemr == 0
replace uemr_cat = 1 if uemr_job == 1
replace uemr_cat = 2 if uemr_fam == 1
replace uemr_cat = 3 if uemr_laidoff == 1
replace uemr_cat = 4 if uemr_vacation == 1
replace uemr_cat = 5 if uemr_edu == 1
replace uemr_cat = 6 if uemr_didnot == 1
replace uemr_cat = 7 if uemr_hlth == 1
replace uemr_cat = 8 if uemr_self == 1
replace uemr_cat = 9 if uemr_retire == 1
replace uemr_cat = 10 if uemr_unk == 1

gen uemr_disagg = uemr
recode uemr_disagg (11=99) (17/max=99)
recode uemr_disagg (99=17)
replace uemr_disagg = uemr_disagg-1 if uemr_disagg>11

/***************
LABELING
***************/

label define uemr 0 "Employed"
label define uemr 1 "Laid off", add
label define uemr 2 "Labor dispute", add
label define uemr 3 "Unpaid vacation", add
label define uemr 4 "No jobs available", add
label define uemr 5 "Bad weather", add
label define uemr 6 "Wanted to take care of family", add
label define uemr 7 "Child care too expensive", add
label define uemr 8 "Pregnancy", add
label define uemr 9 "In job training", add
label define uemr 10 "In school", add
label define uemr 11 "Institutionalized", add
label define uemr 12 "Available wages too low", add
label define uemr 13 "Did not want to work", add
label define uemr 14 "Ill or disabled", add
label define uemr 15 "Self-employed", add
label define uemr 16 "Retired", add
label define uemr 17 "Temporarily ill or disabled", add
label define uemr 18 "Permanently ill or disabled", add
label define uemr 19 "Other", add
label define uemr 99 "Unknown", add
label values uemr uemr

label define uemr_disagg 0 "Employed"
label define uemr_disagg 1 "Laid off", add
label define uemr_disagg 2 "Labor dispute", add
label define uemr_disagg 3 "Unpaid vacation", add
label define uemr_disagg 4 "No jobs available", add
label define uemr_disagg 5 "Bad weather", add
label define uemr_disagg 6 "Wanted to take care of family", add
label define uemr_disagg 7 "Child care too expensive", add
label define uemr_disagg 8 "Pregnancy", add
label define uemr_disagg 9 "In job training", add
label define uemr_disagg 10 "In school", add
label define uemr_disagg 11 "Available wages too low", add
label define uemr_disagg 12 "Did not want to work", add
label define uemr_disagg 13 "Ill or disabled", add
label define uemr_disagg 14 "Self-employed", add
label define uemr_disagg 15 "Retired", add
label define uemr_disagg 16 "Other/unknown", add
label values uemr_disagg uemr_disagg

label define uemr_cat 0 "Employed"
label define uemr_cat 1 "Family", add
label define uemr_cat 2 "Job/work conditions", add
label define uemr_cat 3 "Laid off", add
label define uemr_cat 4 "Unpaid Vacation", add
label define uemr_cat 5 "Education", add
label define uemr_cat 6 "Did not want to work", add
label define uemr_cat 7 "Ill or disabled", add
label define uemr_cat 8 "Self-employed", add
label define uemr_cat 9 "Retired", add
label define uemr_cat 10 "Other/unknown", add
label values uemr_cat uemr_cat

/***************
DATES - Replace dates to make it stata compatable
***************/

replace month = 179 + month if month > 0
replace month = 167 if month == 0
format month %tm

*the dates refer to number of days since january 1, 1974
foreach var of varlist int_date uemst uemnd {
	replace `var' = . if `var' == -999
	replace `var' = 5114 + `var'
	format `var' %td
}

/***************
ATTRITION BY TREATMENT GROUP
***************/

sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*
*drop unique*

*count unique observations
sort famnum id survey month
by famnum id: gen count = _n
by famnum id: replace count = count[_N]
tab count unique_fam

preserve
	collapse (sum) obs=unique_fam, by(count treatment)
	sort treatment count
	by treatment: egen total = sum(obs)
	gen pct = obs/total
	reshape wide obs pct total, i(count) j(treatment)

	rename count survey
	rename obs0 n_control
	rename obs1 n_treatment
	rename pct0 pct_control
	rename pct1 pct_treatment
	rename total0 total_n_control
	rename total1 total_n_treatment
	order survey n_control total_n_control n_treatment total_n_treatment pct_control pct_treatment
	export excel using "$tables/attrition.xls", sheetreplace firstrow(variables)
restore

drop unique*

/***************
SAVE
***************/


*count unique observations
sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*
drop unique*

save "$data_files/unbalanced.dta", replace
keep if attrition == 0 
sum count /*there are a small number of families who appear to drop out, but also have attrition == 0.  i drop these.*/
keep if count == `r(max)'

*The codebook indicates that 590 families complete the experiment (MINC4, pg. 2), 
*but there are 591 families who are present in each wave and do not have a code indicating attrition date or reason.  
*The discrepancy appears to from family number 15459, which is present in each survey wave, 
*does not have a code indicating attrition date or reason, but does not have an interview date after survey wave 8.  
*Therefore, we drop this family from the balanced panel.
drop if famnum == 15459  
save "$data_files/balanced.dta", replace

*count unique observations
sort famnum month
by famnum: gen unique_fam = 1 if _n == 1
sort id month
by id: gen unique_n = 1 if _n == 1
sum unique*
drop unique*


/***************
REMOVE OLD FILES
***************/

rm "$data_files/hd.dta"
rm "$data_files/wf.dta"
rm "$data_files/int_date.dta"

/*
tab survey uemr_any if treatment == 1, row nofreq m
tab survey uemr_any if treatment == 0, row nofreq m
tab survey uemr_any, row nofreq m
tab month uemr_any, row nofreq m
tab month uemr_any if first_m == 1, row nofreq m
tab month uemr_any if first_m == 0, row nofreq m
tab month uemr_any if male == 0, row nofreq m
