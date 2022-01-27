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
Download data files
***************/

*https://dataverse.lib.umanitoba.ca/dataverse/Mincome_Data

* Mincome Baseline Data (Minc1)
* https://doi.org/10.5203/FK2/NV200L
*copy "https://dataverse.lib.umanitoba.ca/api/access/datafile/1001" "$data_files/minc1.xlsx"
		
import excel "$data_files/MINC1.xlsx", sheet("baseline.data") cellrange(A2:CN2179) firstrow clear
rename *, lower /* make variables lowercase */
destring, replace force /* destring */
drop if famnum == . /* delete tabulations at the bottom of excel file */
save "$data_files/minc1.dta", replace

* Mincome Longitudinal Labour Market Data (Minc4)
* https://doi.org/10.5203/FK2/PO1F6R
*copy "https://dataverse.lib.umanitoba.ca/api/access/datafile/1007" "$data_files/minc4.xlsx"

import excel "$data_files/minc4.xlsx", sheet("labour.data") cellrange(A2:LZ1294) firstrow clear
rename *, lower /* make variables lowercase */
destring, replace /* destring */
rename g di3
drop if famnum == .
save "$data_files/MINC4.dta", replace

/***************
Download codebooks
***************/

copy "https://dataverse.lib.umanitoba.ca/api/access/datafile/970" "$support_files/minc1-Baseline-Codebook-1.pdf"

copy "https://dataverse.lib.umanitoba.ca/api/access/datafile/1017" "$support_files/minc4-Longitudinal-Labour File-Codebook.pdf"
